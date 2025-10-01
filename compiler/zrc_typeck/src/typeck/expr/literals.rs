//! type checking for literal expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::{
    ast::ty::Type,
    lexer::{NumberLiteral, StringTok, ZrcString},
};
use zrc_utils::span::{Span, Spannable};

use super::super::scope::Scope;
use crate::{
    tast::{
        expr::{TypedExpr, TypedExprKind},
        ty::Type as TastType,
    },
    typeck::resolve_type,
};

/// Typeck a number literal
pub fn type_expr_number_literal<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    n: NumberLiteral<'input>,
    ty: Option<Type<'input>>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let ty_resolved = ty
        .map(|ty| resolve_type(scope.types, ty))
        .transpose()?
        .unwrap_or(TastType::I32);

    if !ty_resolved.is_integer() {
        return Err(
            DiagnosticKind::InvalidNumberLiteralType(ty_resolved.to_string()).error_in(expr_span),
        );
    }

    // REVIEW: How can we make this work with integers like i128?
    // -4u8 parses as -(4u8) so we don't need to handle negative integers here

    // Check the bounds of the number literal
    // Note: We skip usize/isize since their size is platform-dependent
    let text_without_underscores = n.text_content().replace('_', "");
    let parsed_value = u128::from_str_radix(&text_without_underscores, n.radix())
        .expect("Number literal should have been valid");

    // Check bounds based on type
    #[allow(clippy::wildcard_enum_match_arm)]
    let (min_val, max_val) = match ty_resolved {
        TastType::I8 => (Some(i128::from(i8::MIN)), Some(i128::from(i8::MAX))),
        TastType::U8 => (Some(0), Some(i128::from(u8::MAX))),
        TastType::I16 => (Some(i128::from(i16::MIN)), Some(i128::from(i16::MAX))),
        TastType::U16 => (Some(0), Some(i128::from(u16::MAX))),
        TastType::I32 => (Some(i128::from(i32::MIN)), Some(i128::from(i32::MAX))),
        TastType::U32 => (Some(0), Some(i128::from(u32::MAX))),
        TastType::I64 => (Some(i128::from(i64::MIN)), Some(i128::from(i64::MAX))),
        TastType::U64 => (Some(0), Some(i128::from(u64::MAX))),
        // Skip usize/isize as their size is platform-dependent
        // Also skip all other types (caught by is_integer() check above)
        _ => (None, None),
    };

    if let (Some(min), Some(max)) = (min_val, max_val) {
        // Check if the value fits in the range
        // We need to handle unsigned values that might be larger than i128::MAX
        #[allow(clippy::option_if_let_else)]
        #[allow(clippy::cast_possible_wrap)]
        #[allow(clippy::as_conversions)]
        let value_in_range = if let Ok(max_as_u128) = u128::try_from(i128::MAX) {
            if parsed_value <= max_as_u128 {
                let value_as_signed = parsed_value as i128;
                value_as_signed >= min && value_as_signed <= max
            } else {
                // Value is too large to fit in any signed integer type we support
                false
            }
        } else {
            false
        };

        if !value_in_range {
            return Err(DiagnosticKind::NumberLiteralOutOfBounds(
                n.to_string(),
                ty_resolved.to_string(),
                min.to_string(),
                max.to_string(),
            )
            .error_in(expr_span));
        }
    }

    Ok(TypedExpr {
        inferred_type: ty_resolved.clone(),
        kind: TypedExprKind::NumberLiteral(n, ty_resolved).in_span(expr_span),
    })
}

/// Typeck a str literal
pub fn type_expr_string_literal<'input>(
    _scope: &Scope<'input, '_>,
    expr_span: Span,
    str: ZrcString<'input>,
) -> TypedExpr<'input> {
    TypedExpr {
        inferred_type: TastType::Ptr(Box::new(TastType::U8)),
        kind: TypedExprKind::StringLiteral(str).in_span(expr_span),
    }
}

/// Typeck a char literal
pub fn type_expr_char_literal<'input>(
    _scope: &Scope<'input, '_>,
    expr_span: Span,
    ch: StringTok<'input>,
) -> TypedExpr<'input> {
    TypedExpr {
        inferred_type: TastType::U8,
        kind: TypedExprKind::CharLiteral(ch).in_span(expr_span),
    }
}

/// Resolve an identifier
pub fn type_expr_identifier<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    i: &'input str,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let ty = scope.values.resolve(i).ok_or_else(|| {
        DiagnosticKind::UnableToResolveIdentifier(i.to_string()).error_in(expr_span)
    })?;
    Ok(TypedExpr {
        inferred_type: ty.clone(),
        kind: TypedExprKind::Identifier(i).in_span(expr_span),
    })
}

/// Typeck a boolean literal
pub fn type_expr_boolean_literal<'input>(
    _scope: &Scope<'input, '_>,
    expr_span: Span,
    value: bool,
) -> TypedExpr<'input> {
    TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::BooleanLiteral(value).in_span(expr_span),
    }
}

#[cfg(test)]
mod tests {
    use zrc_parser::{ast::ty::Type as AstType, lexer::NumberLiteral};
    use zrc_utils::{span::Span, spanned};

    use super::*;
    use crate::typeck::scope::GlobalScope;

    /// Helper to create an AST Type from a string identifier
    fn make_ast_type(name: &str) -> AstType<'_> {
        AstType::build_ident(spanned!(0, name, name.len()))
    }

    #[test]
    fn test_number_literal_within_bounds() {
        let global_scope = GlobalScope::new();
        let scope = global_scope.create_subscope();
        let span = Span::from_positions(0, 5);

        // Test valid i8 values
        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Decimal("127"),
                Some(make_ast_type("i8"))
            )
            .is_ok()
        );

        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Decimal("0"),
                Some(make_ast_type("i8"))
            )
            .is_ok()
        );

        // Test valid u8 values
        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Decimal("255"),
                Some(make_ast_type("u8"))
            )
            .is_ok()
        );

        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Decimal("0"),
                Some(make_ast_type("u8"))
            )
            .is_ok()
        );
    }

    #[test]
    fn test_number_literal_out_of_bounds() {
        let global_scope = GlobalScope::new();
        let scope = global_scope.create_subscope();
        let span = Span::from_positions(0, 5);

        // Test i8 overflow
        let result = type_expr_number_literal(
            &scope,
            span,
            NumberLiteral::Decimal("128"),
            Some(make_ast_type("i8")),
        );
        assert!(result.is_err());
        if let Err(diagnostic) = result {
            assert!(matches!(
                diagnostic.1.into_value(),
                DiagnosticKind::NumberLiteralOutOfBounds(_, _, _, _)
            ));
        }

        // Test u8 overflow
        let result = type_expr_number_literal(
            &scope,
            span,
            NumberLiteral::Decimal("256"),
            Some(make_ast_type("u8")),
        );
        assert!(result.is_err());
        if let Err(diagnostic) = result {
            assert!(matches!(
                diagnostic.1.into_value(),
                DiagnosticKind::NumberLiteralOutOfBounds(_, _, _, _)
            ));
        }

        // Test i32 default overflow
        let result = type_expr_number_literal(
            &scope,
            span,
            NumberLiteral::Decimal("5000000000"),
            None, // defaults to i32
        );
        assert!(result.is_err());
        if let Err(diagnostic) = result {
            assert!(matches!(
                diagnostic.1.into_value(),
                DiagnosticKind::NumberLiteralOutOfBounds(_, _, _, _)
            ));
        }
    }

    #[test]
    fn test_number_literal_hex_and_binary() {
        let global_scope = GlobalScope::new();
        let scope = global_scope.create_subscope();
        let span = Span::from_positions(0, 5);

        // Test valid hex
        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Hexadecimal("FF"),
                Some(make_ast_type("u8"))
            )
            .is_ok()
        );

        // Test invalid hex
        let result = type_expr_number_literal(
            &scope,
            span,
            NumberLiteral::Hexadecimal("100"),
            Some(make_ast_type("u8")),
        );
        assert!(result.is_err());

        // Test valid binary
        assert!(
            type_expr_number_literal(
                &scope,
                span,
                NumberLiteral::Binary("11111111"),
                Some(make_ast_type("u8"))
            )
            .is_ok()
        );

        // Test invalid binary
        let result = type_expr_number_literal(
            &scope,
            span,
            NumberLiteral::Binary("100000000"),
            Some(make_ast_type("u8")),
        );
        assert!(result.is_err());
    }
}
