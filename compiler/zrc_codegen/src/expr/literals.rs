//! code generation for literal expressions

use inkwell::{
    types::StringRadix,
    values::{BasicValue, BasicValueEnum},
};
use zrc_typeck::tast::expr::{NumberLiteral, Place, PlaceKind, StringTok, ZrcString};
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
