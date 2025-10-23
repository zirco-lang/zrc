//! code generation for access, assignment, and ref/deref expressions

use inkwell::values::{BasicValue, BasicValueEnum};
use zrc_typeck::tast::expr::{Place, PlaceKind, TypedExpr};
use zrc_utils::span::{Spannable, Spanned};

use super::place::cg_place;
use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::{CgExprArgs, cg_expr},
    ty::llvm_basic_type,
    unpack,
};

/// Generate LLVM IR for an index expression
pub fn cg_index<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        expr_span,
        inferred_type,
    }: CgExprArgs<'ctx, 'input, '_>,
    ptr: Box<TypedExpr<'input>>,
    idx: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let ptr = unpack!(
        bb = cg_place(
            cg,
            bb,
            Place {
                inferred_type: inferred_type.clone(),
                kind: PlaceKind::Index(ptr, idx).in_span(expr_span),
            },
        )
    );

    let loaded = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, ptr, "load")
        .expect("index load should have compiled successfully");

    bb.and(loaded.as_basic_value_enum())
}

/// Generate LLVM IR for a dot expression
pub fn cg_dot<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        expr_span,
        inferred_type,
    }: CgExprArgs<'ctx, 'input, '_>,
    place: Box<Place<'input>>,
    key: Spanned<&'input str>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let ptr = unpack!(
        bb = cg_place(
            cg,
            bb,
            Place {
                inferred_type: inferred_type.clone(),
                kind: PlaceKind::Dot(place, key).in_span(expr_span),
            },
        )
    );

    let loaded = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, ptr, "load")
        .expect("dot load should have compiled successfully");

    bb.and(loaded.as_basic_value_enum())
}

/// Generate LLVM IR for a dereference expression
pub fn cg_deref<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    ptr: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let ptr = unpack!(bb = cg_expr(cg, bb, *ptr));

    let reg = cg
        .builder
        .build_load(
            llvm_basic_type(&cg, &inferred_type).0,
            ptr.into_pointer_value(),
            "load",
        )
        .expect("dereference should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

/// Generate LLVM IR for an assignment expression
pub fn cg_assignment<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    place: Place<'input>,
    value: Box<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let value = unpack!(bb = cg_expr(cg, bb, *value));
    let place = unpack!(bb = cg_place(cg, bb, place));

    cg.builder
        .build_store(place, value)
        .expect("store instruction in assignment should have built successfully");

    bb.and(value)
}

/// Generate LLVM IR for an address-of expression
pub fn cg_address_of<'ctx, 'input>(
    CgExprArgs { cg, mut bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    x: Place<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let value = unpack!(bb = cg_place(cg, bb, x));

    bb.and(value.as_basic_value_enum())
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn pointer_indexing_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                fn take_int(x: i32);

                fn test() {
                    let x: *i32;

                    // TEST: `x` is *i32, so %let_x is a **i32 (ptr to the stack).
                    // %let_x needs to be GEP'd into and the value `i32` at idx 4 must be loaded.
                    take_int(x[4 as usize]);
                }
            "});
    }

    #[test]
    fn pointer_deref_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                fn test() -> i32 {
                    let x: *i32;

                    // TEST: should load twice, once to read the value of `x` and once to read what
                    // it points to
                    return *x;
                }
            "});
    }

    #[test]
    fn struct_property_access_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                struct S { x: i32, y: i32 }
                fn take_int(x: i32);

                fn test() {
                    let x: S;

                    // TEST: should GEP into `x` to get the second property (`y`) and then
                    // load that value and call take_int
                    take_int(x.y);
                }
            "});
    }

    #[test]
    fn union_property_access_in_expr_position() {
        cg_snapshot_test!(indoc! {"
                union U { x: i32, y: i8 }
                fn take_i32(x: i32);
                fn take_i8(x: i8);

                fn test() {
                    let x: U;

                    // TEST: the pointer is cast and then read from as an i32
                    take_i32(x.x);

                    // TEST: the pointer is cast and then read from as an i8
                    take_i8(x.y);
                }
            "});
    }
}
