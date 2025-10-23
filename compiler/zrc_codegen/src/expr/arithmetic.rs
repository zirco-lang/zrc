//! code generation for arithmetic expressions

use inkwell::{
    builder::BuilderError,
    values::{BasicValue, BasicValueEnum, IntValue},
};
use zrc_typeck::tast::{
    expr::{Arithmetic, BinaryBitwise, TypedExpr},
    ty::Type,
};

use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    ctx::BlockCtx,
    expr::{CgExprArgs, cg_expr},
    ty::llvm_basic_type,
    unpack,
};

/// Build the required instruction for a [`BinaryBitwise`] operation
pub fn build_binary_bitwise<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    op: BinaryBitwise,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    result_is_signed: bool,
) -> Result<IntValue<'ctx>, BuilderError> {
    match op {
        BinaryBitwise::And => cg.builder.build_and(lhs, rhs, "and"),
        BinaryBitwise::Or => cg.builder.build_or(lhs, rhs, "or"),
        BinaryBitwise::Xor => cg.builder.build_xor(lhs, rhs, "xor"),
        BinaryBitwise::Shl => cg.builder.build_left_shift(lhs, rhs, "shl"),
        BinaryBitwise::Shr => cg
            .builder
            .build_right_shift(lhs, rhs, result_is_signed, "shr"),
    }
}
/// Build the required instruction for an [`Arithmetic`] operation
pub fn build_arithmetic<'ctx>(
    cg: BlockCtx<'ctx, '_, '_>,
    op: Arithmetic,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    result_is_signed: bool,
) -> Result<IntValue<'ctx>, BuilderError> {
    match op {
        Arithmetic::Addition => cg.builder.build_int_add(lhs, rhs, "add"),
        Arithmetic::Subtraction => cg.builder.build_int_sub(lhs, rhs, "sub"),
        Arithmetic::Multiplication => cg.builder.build_int_mul(lhs, rhs, "mul"),

        Arithmetic::Division if result_is_signed => {
            cg.builder.build_int_signed_div(lhs, rhs, "div")
        }
        Arithmetic::Division => cg.builder.build_int_unsigned_div(lhs, rhs, "div"),

        Arithmetic::Modulo if result_is_signed => cg.builder.build_int_signed_rem(lhs, rhs, "rem"),
        Arithmetic::Modulo => cg.builder.build_int_unsigned_rem(lhs, rhs, "rem"),
    }
}

/// Code generate a binary bitwise operation
pub fn cg_binary_bitwise<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    op: BinaryBitwise,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
    let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

    let reg = build_binary_bitwise(
        cg,
        op,
        lhs.into_int_value(),
        rhs.into_int_value(),
        inferred_type.is_signed_integer(),
    )
    .expect("binary bitwise operation should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

/// Code generate an arithmetic operation
pub fn cg_arithmetic<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    op: Arithmetic,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let lhs_ty = lhs.inferred_type.clone();
    let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
    let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

    if let Type::Ptr(pointee) = lhs_ty {
        // Most languages make incrementing a pointer increase the address by the size
        // of the pointee type, hence our use of `gep`.
        // REVIEW: Is this the approach we want to take?
        #[allow(clippy::wildcard_enum_match_arm)]
        let reg = match op {
            // SAFETY: This can segfault if indices are used incorrectly
            // This is only used for pointer arithmetic, so the indices should be correct
            Arithmetic::Addition => unsafe {
                cg.builder.build_gep(
                    llvm_basic_type(&cg, &pointee).0,
                    lhs.into_pointer_value(),
                    &[rhs.into_int_value()],
                    "ptr_add",
                )
            },
            Arithmetic::Subtraction => {
                let rhs = cg
                    .builder
                    .build_int_neg(rhs.into_int_value(), "neg")
                    .expect("negation should have compiled successfully")
                    .as_basic_value_enum();

                // SAFETY: This can segfault if indices are used incorrectly
                // This is only used for pointer arithmetic, so the indices should be
                // correct
                unsafe {
                    cg.builder.build_gep(
                        llvm_basic_type(&cg, &pointee).0,
                        lhs.into_pointer_value(),
                        &[rhs.into_int_value()],
                        "ptr_sub",
                    )
                }
            }
            _ => panic!("invalid pointer arithmetic operation"),
        }
        .expect("pointer arithmetic should have compiled successfully");

        bb.and(reg.as_basic_value_enum())
    } else {
        let reg = build_arithmetic(
            cg,
            op,
            lhs.into_int_value(),
            rhs.into_int_value(),
            inferred_type.is_signed_integer(),
        )
        .expect("arithmetic operation should have compiled successfully");

        bb.and(reg.as_basic_value_enum())
    }
}

/// Code generate a unary bitwise NOT operation
pub fn cg_unary_bitwise_not<'ctx, 'input>(
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

/// Code generate a unary minus operation
pub fn cg_unary_minus<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    x: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let value = unpack!(bb = cg_expr(cg, bb, *x));

    let reg = cg
        .builder
        .build_int_neg(value.into_int_value(), "neg")
        .expect("negation should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn pointer_arithmetic_generates_proper_gep() {
        cg_snapshot_test!(indoc! {r#"
                fn test() {
                    let x: *i32;

                    // TEST: should create a GEP that is the same as "x[4]"
                    let y = x + 4 as usize;
                    // TEST: and the same, with -4:
                    let z = x - 4 as usize;
                }
            "#});
    }

    #[test]
    fn arithmetic_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_int() -> i32;
                fn get_uint() -> u32;

                fn test() {
                    let sx = get_int();
                    let sy = get_int();
                    let ux = get_uint();
                    let uy = get_uint();

                    // TEST: should create an `add i32` instruction
                    let s_add = sx + sy;
                    let u_add = ux + uy;

                    // TEST: should create a `sub i32` instruction
                    let s_sub = sx - sy;
                    let u_sub = ux - uy;

                    // TEST: should create a `mul i32` instruction
                    let s_mul = sx * sy;
                    let u_mul = ux * uy;

                    // TEST: should create a `sdiv i32` instruction
                    let s_div = sx / sy;
                    // TEST: should create a `udiv i32` instruction
                    let u_div = ux / uy;

                    // TEST: should create a `srem i32` instruction
                    let s_rem = sx % sy;
                    // TEST: should create a `urem i32` instruction
                    let u_rem = ux % uy;
                }
            "});
    }

    #[test]
    fn bitwise_operators_generate() {
        cg_snapshot_test!(indoc! {"
                fn get_int() -> i32;
                fn get_uint() -> u32;

                fn test() {
                    let x = get_int();
                    let y = get_int();
                    let u = get_uint();

                    // TEST: should create a `not i32` instruction
                    let not = ~x;

                    // TEST: should create a `and i32` instruction
                    let and = x & y;

                    // TEST: should create a `or i32` instruction
                    let or = x | y;

                    // TEST: should create a `xor i32` instruction
                    let xor = x ^ y;

                    // TEST: should create a `shl i32` instruction
                    let shl = x << u;

                    // TEST: should create a `lshr i32` instruction
                    let lshr = u >> u;

                    // TEST: should create a `ashr i32` instruction (as the lhs is signed)
                    let ashr = x >> u;
                }
            "});
    }
}
