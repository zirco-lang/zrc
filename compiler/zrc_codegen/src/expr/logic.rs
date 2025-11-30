//! code generation for logical expressions

use inkwell::{
    IntPredicate,
    values::{BasicValue, BasicValueEnum},
};
use zrc_typeck::tast::expr::{Comparison, Equality, Logical, TypedExpr};

use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::{CgExprArgs, cg_expr},
    unpack,
};

/// Get the [`IntPredicate`] for an [`Equality`] operation
const fn int_predicate_for_equality(op: Equality) -> IntPredicate {
    match op {
        Equality::Eq => IntPredicate::EQ,
        Equality::Neq => IntPredicate::NE,
    }
}
/// Get the [`IntPredicate`] for a [`Comparison`] operation
const fn int_predicate_for_comparison(op: Comparison, signed: bool) -> IntPredicate {
    match (op, signed) {
        (Comparison::Lt, true) => IntPredicate::SLT,
        (Comparison::Lt, false) => IntPredicate::ULT,
        (Comparison::Gt, true) => IntPredicate::SGT,
        (Comparison::Gt, false) => IntPredicate::UGT,
        (Comparison::Lte, true) => IntPredicate::SLE,
        (Comparison::Lte, false) => IntPredicate::ULE,
        (Comparison::Gte, true) => IntPredicate::SGE,
        (Comparison::Gte, false) => IntPredicate::UGE,
    }
}

/// Code generate an equality expression
pub fn cg_equality<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    op: Equality,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
    let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

    // For pointer comparisons, convert pointers to integers first
    let (lhs_int, rhs_int) = if lhs.is_pointer_value() && rhs.is_pointer_value() {
        let target_data = cg.target_machine.get_target_data();
        let ptr_sized_int = cg.ctx.ptr_sized_int_type(&target_data, None);
        let lhs_int = cg
            .builder
            .build_ptr_to_int(lhs.into_pointer_value(), ptr_sized_int, "ptr_to_int")
            .expect("ptr_to_int should have compiled successfully");
        let rhs_int = cg
            .builder
            .build_ptr_to_int(rhs.into_pointer_value(), ptr_sized_int, "ptr_to_int")
            .expect("ptr_to_int should have compiled successfully");
        (lhs_int, rhs_int)
    } else {
        (lhs.into_int_value(), rhs.into_int_value())
    };

    let reg = cg
        .builder
        .build_int_compare(int_predicate_for_equality(op), lhs_int, rhs_int, "cmp")
        .expect("equality comparison should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

/// Code generate a comparison expression
pub fn cg_comparison<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    op: Comparison,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
    let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

    let reg = cg
        .builder
        .build_int_compare(
            int_predicate_for_comparison(op, inferred_type.is_signed_integer()),
            lhs.into_int_value(),
            rhs.into_int_value(),
            "cmp",
        )
        .expect("comparison should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

/// Code generate a logical AND expression
pub fn cg_logical_and<'ctx, 'input>(
    CgExprArgs {
        cg, bb: mut top_bb, ..
    }: CgExprArgs<'ctx, 'input, '_>,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    // Because Zirco short circuits logical expressions, we generate LLVM like this:
    // Hence ensuring that rhs is only evaluated if lhs is true.
    //
    // %lhs = (cg_expr lhs)
    // br i1 %lhs label %land_rhs, label %land_end
    // land_rhs:
    //   %rhs = (cg_expr rhs)
    //   br label %land_end
    // land_end:
    //   ; If we branched directly from entry, lhs was false, so result is false
    //   ; If we branched from land_rhs, result is whatever rhs evaluated to
    //   %result = phi i1 [ false, %entry ], [ %rhs, %land_rhs ]

    let mut land_rhs_bb = cg.ctx.append_basic_block(cg.fn_value, "land_rhs");
    let land_end_bb = cg.ctx.append_basic_block(cg.fn_value, "land_end");

    let lhs = unpack!(top_bb = cg_expr(cg, top_bb, *lhs));

    cg.builder
        .build_conditional_branch(lhs.into_int_value(), land_rhs_bb, land_end_bb)
        .expect(
            "conditional branch to logical AND rhs or end block should have compiled successfully",
        );
    cg.builder.position_at_end(land_rhs_bb);

    let rhs = unpack!(land_rhs_bb = cg_expr(cg, land_rhs_bb, *rhs));

    cg.builder
        .build_unconditional_branch(land_end_bb)
        .expect("unconditional branch to logical AND end block should have compiled successfully");
    cg.builder.position_at_end(land_end_bb);

    let reg = cg
        .builder
        .build_phi(cg.ctx.bool_type(), "land_result")
        .expect("phi node should have compiled successfully");

    reg.add_incoming(&[
        (&cg.ctx.bool_type().const_int(0, false), top_bb),
        (&rhs, land_rhs_bb),
    ]);

    land_end_bb.and(reg.as_basic_value())
}

/// Code generate a logical OR expression
pub fn cg_logical_or<'ctx, 'input>(
    CgExprArgs {
        cg, bb: mut top_bb, ..
    }: CgExprArgs<'ctx, 'input, '_>,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    // Because Zirco short circuits logical expressions, we generate LLVM like this:
    // Hence ensuring that rhs is only evaluated if lhs is false.
    //
    // %lhs = (cg_expr lhs)
    // br i1 %lhs label %lor_end, label %lor_rhs
    // lor_rhs:
    //   %rhs = (cg_expr rhs)
    //   br label %lor_end
    // lor_end:
    //   ; If we branched directly from entry, lhs was true, so result is true
    //   ; If we branched from lor_rhs, result is whatever rhs evaluated to
    //   %result = phi i1 [ true, %entry ], [ %rhs, %lor_rhs ]

    let mut lor_rhs_bb = cg.ctx.append_basic_block(cg.fn_value, "lor_rhs");
    let lor_end_bb = cg.ctx.append_basic_block(cg.fn_value, "lor_end");

    let lhs = unpack!(top_bb = cg_expr(cg, top_bb, *lhs));
    cg.builder
        .build_conditional_branch(lhs.into_int_value(), lor_end_bb, lor_rhs_bb)
        .expect(
            "conditional branch to logical OR rhs or end block should have compiled successfully",
        );
    cg.builder.position_at_end(lor_rhs_bb);

    let rhs = unpack!(lor_rhs_bb = cg_expr(cg, lor_rhs_bb, *rhs));
    cg.builder
        .build_unconditional_branch(lor_end_bb)
        .expect("unconditional branch to logical OR end block should have compiled successfully");
    cg.builder.position_at_end(lor_end_bb);

    let reg = cg
        .builder
        .build_phi(cg.ctx.bool_type(), "lor_result")
        .expect("phi node should have compiled successfully");

    reg.add_incoming(&[
        (&cg.ctx.bool_type().const_int(1, false), top_bb),
        (&rhs, lor_rhs_bb),
    ]);

    lor_end_bb.and(reg.as_basic_value())
}

/// Code generate a logical expression
pub fn cg_logical<'ctx, 'input>(
    args: CgExprArgs<'ctx, 'input, '_>,
    op: Logical,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    match op {
        Logical::And => cg_logical_and(args, lhs, rhs),
        Logical::Or => cg_logical_or(args, lhs, rhs),
    }
}

/// Code generate a unary logical NOT operation
pub fn cg_unary_not<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    x: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let value = unpack!(bb = cg_expr(cg, bb, *x));

    let reg = cg
        .builder
        .build_not(value.into_int_value(), "not")
        .expect("not should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn equality_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;

                fn test() {
                    let a = get_bool();
                    let b = get_bool();

                    // TEST: should create an `icmp eq` instruction
                    let eq = a == b;

                    // TEST: should create an `icmp ne` instruction
                    let ne = a != b;
                }
            "});
    }

    #[test]
    fn logical_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_bool() -> bool;

                fn test() {
                    let a = get_bool();
                    let b = get_bool();

                    // TEST: should create a bit AND
                    let and = a && b;

                    // TEST: should create a bit OR
                    let or = a || b;

                    // TEST: should create a bit NOT
                    let not = !a;
                }
            "});
    }

    #[test]
    fn pointer_equality_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_ptr() -> *i32;

                fn test() {
                    let a = get_ptr();
                    let b = get_ptr();

                    // TEST: should create pointer comparison using ptrtoint + icmp
                    let eq = a == b;

                    // TEST: should create pointer comparison using ptrtoint + icmp
                    let ne = a != b;
                }
            "});
    }
}
