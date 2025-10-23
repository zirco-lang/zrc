//! code generation for control flow expressions

use inkwell::values::{BasicValue, BasicValueEnum};
use zrc_typeck::tast::expr::{Place, TypedExpr};

use super::place::cg_place;
use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::{CgExprArgs, cg_expr},
    ty::{llvm_basic_type, llvm_type},
    unpack,
};

/// Code generate a comma expression
pub fn cg_comma<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let _ = unpack!(bb = cg_expr(cg, bb, *lhs));
    cg_expr(cg, bb, *rhs)
}

/// Code generate a function call expression
pub fn cg_call<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    f: Place<'input>,
    args: Vec<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let llvm_f_type = llvm_type(&cg, &f.inferred_type).0.into_function_type();

    // will always be a function pointer
    let f_ptr = unpack!(bb = cg_place(cg, bb, f));

    let mut bb = bb;
    let old_args = args;
    let mut args = vec![];
    for arg in old_args {
        let new_arg = unpack!(bb = cg_expr(cg, bb, arg));
        args.push(new_arg.into());
    }

    let ret = cg
        .builder
        .build_indirect_call(llvm_f_type, f_ptr, &args, "call")
        .expect("call should have compiled successfully");

    bb.and(if ret.try_as_basic_value().is_left() {
        ret.try_as_basic_value().unwrap_left()
    } else {
        cg.ctx.i8_type().get_undef().as_basic_value_enum()
    })
}

/// Code generate a ternary expression
pub fn cg_ternary<'ctx, 'input>(
    CgExprArgs {
        cg,
        bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    cond: Box<TypedExpr<'input>>,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let cond = cg_expr(cg, bb, *cond).into_value();

    // If lhs and rhs are registers, the code generated will look like:
    //   entry:
    //       ...
    //       %cond = ...
    //       br i1 %cond, label %if_true, label %if_false
    //   if_true:
    //       ... yields %lhs
    //       br label %end
    //   if_false:
    //       ... yields %rhs
    //       br label %end
    //   end:
    //       %yield = phi TY [ %lhs, %if_true ], [ %rhs, %if_false ]
    //
    // If they are constants, the code generated will load as:
    //   entry:
    //       ...
    //       %cond = ...
    //       br i1 %cond, label %if_true, label %if_false
    //   if_true:
    //       br label %end
    //   if_false:
    //       br label %end
    //   end:
    //       %yield = phi TY [ LHS VALUE, %if_true ], [ RHS VALUE, %if_false ]
    //
    // This is just a weird way to `select` and will be inlined.

    let mut if_true_bb = cg.ctx.append_basic_block(cg.fn_value, "if_true");
    let mut if_false_bb = cg.ctx.append_basic_block(cg.fn_value, "if_false");
    let end_bb = cg.ctx.append_basic_block(cg.fn_value, "end");

    cg.builder
        .build_conditional_branch(cond.into_int_value(), if_true_bb, if_false_bb)
        .expect("conditional branch should have been created successfully");

    cg.builder.position_at_end(if_true_bb);
    let if_true = unpack!(if_true_bb = cg_expr(cg, if_true_bb, *lhs));
    cg.builder
        .build_unconditional_branch(end_bb)
        .expect("unconditional branch should have been created successfully");

    cg.builder.position_at_end(if_false_bb);
    let if_false = unpack!(if_false_bb = cg_expr(cg, if_false_bb, *rhs));
    cg.builder
        .build_unconditional_branch(end_bb)
        .expect("unconditional branch should have been created successfully");

    cg.builder.position_at_end(end_bb);
    let result_reg = cg
        .builder
        .build_phi(llvm_basic_type(&cg, &inferred_type).0, "yield")
        .expect("phi node should have been created successfully");
    result_reg.add_incoming(&[(&if_true, if_true_bb), (&if_false, if_false_bb)]);

    end_bb.and(result_reg.as_basic_value())
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn comma_yields_right_value() {
        cg_snapshot_test!(indoc! {"
                fn f();
                fn g() -> i32;

                fn test() -> i32 {
                    // TEST: f() is run, g() is run and used as the return value
                    return f(), g();
                }
            "});
    }

    #[test]
    fn ternary_operations_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;
                fn get_int() -> i32;
                fn take_int(x: i32);
                fn test() {
                    // TEST: should produce a proper diamond-shaped cfg
                    let num = get_bool() ? get_int() : 3;
                    take_int(num);
                }
            "});
    }
}
