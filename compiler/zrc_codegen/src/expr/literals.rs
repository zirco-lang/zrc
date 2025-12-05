//! code generation for literal expressions

use inkwell::{
    types::StringRadix,
    values::{BasicValue, BasicValueEnum},
};
use zrc_typeck::tast::{
    expr::{NumberLiteral, Place, PlaceKind, StringTok, TypedExpr, ZrcString},
    ty::Type,
};
use zrc_utils::span::Spannable;

use super::{cg_expr, place::cg_place};
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

/// Code generate an array literal
pub fn cg_array_literal<'ctx, 'input>(
    ce: CgExprArgs<'ctx, 'input, '_>,
    elements: Vec<TypedExpr<'input>>,
) -> BasicBlockAnd<'ctx, BasicValueEnum<'ctx>> {
    let CgExprArgs {
        cg,
        mut bb,
        inferred_type,
        ..
    } = ce;

    // Extract array size and element type
    let (_size, _element_type) = match &inferred_type {
        Type::Array { size, element_type } => (*size, element_type.as_ref()),
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::I32
        | Type::U32
        | Type::I64
        | Type::U64
        | Type::Usize
        | Type::Isize
        | Type::Bool
        | Type::Int
        | Type::Ptr(_)
        | Type::Fn(_)
        | Type::Struct(_)
        | Type::Union(_)
        | Type::Opaque(_) => panic!("array literal must have array type"),
    };

    // Create an alloca for the array
    let array_type = llvm_basic_type(&cg, &inferred_type).0;
    let array_alloca = cg
        .builder
        .build_alloca(array_type, "array_literal")
        .expect("alloca should succeed");

    // Store each element
    for (idx, elem) in elements.into_iter().enumerate() {
        let elem_value = unpack!(bb = cg_expr(cg, bb, elem));

        // Get pointer to the array element using GEP
        let zero = cg.ctx.i64_type().const_zero();
        #[expect(clippy::as_conversions)]
        let idx_val = cg.ctx.i64_type().const_int(idx as u64, false);

        // SAFETY: We're using GEP to get a pointer to an array element,
        // which is safe as long as the index is within bounds (which it is by
        // construction)
        let elem_ptr = unsafe {
            cg.builder
                .build_gep(array_type, array_alloca, &[zero, idx_val], "array_elem_ptr")
        }
        .expect("GEP should succeed");

        cg.builder
            .build_store(elem_ptr, elem_value)
            .expect("store should succeed");
    }

    // Return the array value (loaded from alloca)
    let array_value = cg
        .builder
        .build_load(array_type, array_alloca, "array_value")
        .expect("load should succeed");

    bb.and(array_value.as_basic_value_enum())
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
}
