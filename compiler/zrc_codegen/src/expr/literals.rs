//! code generation for literal expressions

use inkwell::{
    types::StringRadix,
    values::{BasicValue, BasicValueEnum},
};
use zrc_typeck::tast::expr::{NumberLiteral, Place, PlaceKind, StringTok, TypedExpr, ZrcString};
use zrc_utils::span::Spannable;

use super::place::cg_place;
use crate::{
    bb::{BasicBlockAnd, BasicBlockExt},
    expr::CgExprArgs,
    ty::{llvm_basic_type, llvm_int_type},
    unpack,
};

/// Get a [`NumberLiteral`]'s [`StringRadix`]
const fn number_literal_radix(n: &NumberLiteral) -> StringRadix {
    match n {
        NumberLiteral::Decimal(_) => StringRadix::Decimal,
        NumberLiteral::Binary(_) => StringRadix::Binary,
        NumberLiteral::Hexadecimal(_) => StringRadix::Hexadecimal,
    }
}

/// Generate LLVM IR for a number literal
pub fn cg_number_literal<'ctx, 'input>(
    CgExprArgs {
        cg,
        bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    n: &NumberLiteral<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let no_underscores = n.text_content().replace('_', "");

    bb.and(
        llvm_int_type(&cg, &inferred_type)
            .0
            .const_int_from_string(&no_underscores, number_literal_radix(n))
            .expect("number literal should have parsed correctly")
            .as_basic_value_enum(),
    )
}

/// Generate LLVM IR for a string literal
pub fn cg_string_literal<'ctx, 'input>(
    CgExprArgs { cg, bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    str: &ZrcString<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    bb.and(
        cg.builder
            .build_global_string_ptr(&str.as_bytes(), "str")
            .expect("string should have built successfully")
            .as_basic_value_enum(),
    )
}

/// Generate LLVM IR for a char literal
pub fn cg_char_literal<'ctx, 'input>(
    CgExprArgs { cg, bb, .. }: CgExprArgs<'ctx, 'input, '_>,
    ch: &StringTok<'input>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    bb.and(
        cg.ctx
            .i8_type()
            .const_int(ch.as_byte().into(), false)
            .as_basic_value_enum(),
    )
}

/// Generate LLVM IR for a boolean literal
pub fn cg_boolean_literal<'ctx>(
    CgExprArgs { cg, bb, .. }: CgExprArgs<'ctx, '_, '_>,
    value: bool,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    bb.and(
        cg.ctx
            .bool_type()
            .const_int(value.into(), false)
            .as_basic_value_enum(),
    )
}

/// Generate LLVM IR for an identifier
pub fn cg_identifier<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        expr_span,
        inferred_type,
    }: CgExprArgs<'ctx, 'input, '_>,
    id: &'input str,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let place = unpack!(
        bb = cg_place(
            cg,
            bb,
            Place {
                inferred_type: inferred_type.clone(),
                kind: PlaceKind::Variable(id).in_span(expr_span),
            },
        )
    );

    let reg = cg
        .builder
        .build_load(llvm_basic_type(&cg, &inferred_type).0, place, "load")
        .expect("ident load should have built successfully");

    bb.and(reg.as_basic_value_enum())
}

/// Generate LLVM IR for an array literal
pub fn cg_array_literal<'ctx, 'input>(
    CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    }: CgExprArgs<'ctx, 'input, '_>,
    elements: Vec<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    use zrc_typeck::tast::ty::Type;

    // Obtain the LLVM array type
    let array_basic = llvm_basic_type(&cg, &inferred_type).0;
    let array_type = array_basic.into_array_type();

    // Allocate on the stack
    let array_ptr = cg
        .builder
        .build_alloca(array_type, "array_tmp")
        .expect("array allocation should have compiled successfully");

    // Extract element type so we can GEP into elements
    let element_type = match &inferred_type {
        Type::Array { element_type, .. } => element_type.as_ref(),
        _ => panic!("array literal has non-array inferred type"),
    };

    let elem_basic = llvm_basic_type(&cg, element_type).0;

    // Bitcast the `ptr to [N x T]` into a `ptr to T` so we can index elements
    // with a single index. This avoids mismatches in the GEP element type.
    use inkwell::AddressSpace;

    // LLVM 15+ uses opaque pointers; use the context's pointer type as the
    // destination for the pointer cast rather than trying to construct a
    // pointer type from the `BasicTypeEnum` (which is deprecated / awkward).
    let elem_ptr_type = cg.ctx.ptr_type(AddressSpace::default());
    let array_as_elem_ptr = cg
        .builder
        .build_pointer_cast(array_ptr, elem_ptr_type, "array_as_elem_ptr")
        .expect("pointer cast should succeed");

    for (i, el) in elements.into_iter().enumerate() {
        let el_val = unpack!(bb = super::cg_expr(cg, bb, el));

        // Use pointer-sized index for indexing
        let idx_const = cg
            .ctx
            .ptr_sized_int_type(&cg.target_machine.get_target_data(), None)
            .const_int(i as u64, false);

        // SAFETY: indices are known-good compile-time constants and the
        // casted pointer points at the first element of the array allocation.
        let gep = unsafe {
            cg.builder
                .build_gep(elem_basic, array_as_elem_ptr, &[idx_const], "elem_ptr")
        }
        .expect("building GEP for array element should succeed");

        let gep_ptr = gep.as_basic_value_enum().into_pointer_value();

        cg.builder
            .build_store(gep_ptr, el_val)
            .expect("store should have compiled successfully");
    }

    let reg = cg
        .builder
        .build_load(array_type, array_ptr, "array_val")
        .expect("load should have compiled successfully");

    bb.and(reg.as_basic_value_enum())
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn typed_integers_generate_properly() {
        cg_snapshot_test!(indoc! {"
                fn test() -> i8 {
                    // TEST: returns `i8`
                    return 4i8;
                }
            "});
    }

    #[test]
    fn string_literal_escapes_generate() {
        cg_snapshot_test!(indoc! {r#"
                fn test() {
                    // TEST: should properly generate \xNN for each escape
                    let x = "\n\r\t\\\"\x41\u{2400}\0";
                }
            "#});
    }

    /// Tests to ensure non-decimal integer literals
    /// 1. don't panic
    /// 2. are valid.
    ///
    /// <https://github.com/zirco-lang/zrc/issues/119>
    #[test]
    fn regression_119_special_integer_literals() {
        cg_snapshot_test!(indoc! {"
                fn test() {
                    let a = 0b10_10;
                    let b = 0x1F_A4;
                }
            "});
    }

    #[test]
    fn array_literal_generates_properly() {
        cg_snapshot_test!(indoc! {"
                fn test() -> i32 {
                    let x = [1i32, 2i32, 3i32];
                    return x[1usize];
                }
            "});
    }
}
