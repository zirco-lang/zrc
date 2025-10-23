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

    let reg = cg
        .builder
        .build_int_compare(
            int_predicate_for_equality(op),
            lhs.into_int_value(),
            rhs.into_int_value(),
            "cmp",
        )
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

/// Code generate a logical expression
pub fn cg_logical<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    op: Logical,
    lhs: Box<TypedExpr<'input>>,
    rhs: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let lhs = unpack!(bb = cg_expr(cg, bb, *lhs));
    let rhs = unpack!(bb = cg_expr(cg, bb, *rhs));

    let reg = match op {
        Logical::And => cg
            .builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "and"),
        Logical::Or => cg
            .builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "or"),
    }
    .expect("logical operation should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
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
}
