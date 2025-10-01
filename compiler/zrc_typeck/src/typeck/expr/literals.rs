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
    // let parsed_integer = u64::from_str_radix(&n.text_content().replace('_', ""),
    // n.radix())     .expect("Number literal should have been valid");

    // TODO: Check the bounds of each number literal, instead
    // of just panicking or overflowing at runtime (what really happens?)
    //
    // This is difficult because we don't know the size of `usize`/`isize` here.

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
