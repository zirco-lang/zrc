//! type checking for unary operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::Expr;
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{expect, expect_is_integer, expect_is_signed_integer, expr_to_place},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck a unary not
pub fn type_expr_unary_not<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_ty = type_expr(scope, x)?;

    expect(
        x_ty.inferred_type == TastType::Bool,
        "boolean".to_string(),
        x_ty.inferred_type.to_string(),
        expr_span,
    )?;

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryNot(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a bit not
pub fn type_expr_unary_bitwise_not<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = type_expr(scope, x)?;

    expect_is_integer(&x_ty.inferred_type, x_span)?;

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a unary minus
pub fn type_expr_unary_minus<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = type_expr(scope, x)?;

    expect_is_signed_integer(&x_ty.inferred_type, x_span)?;

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryMinus(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a unary ref
pub fn type_expr_unary_address_of<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_ty = type_expr(scope, x)?;

    Ok(TypedExpr {
        inferred_type: TastType::Ptr(Box::new(x_ty.inferred_type.clone())),
        kind: TypedExprKind::UnaryAddressOf(Box::new(expr_to_place(expr_span, x_ty)?))
            .in_span(expr_span),
    })
}

/// Typeck a unary deref
pub fn type_expr_unary_dereference<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_ty = type_expr(scope, x)?;

    if let TastType::Ptr(tt) = x_ty.inferred_type.clone() {
        Ok(TypedExpr {
            inferred_type: *tt,
            kind: TypedExprKind::UnaryDereference(Box::new(x_ty)).in_span(expr_span),
        })
    } else {
        Err(
            DiagnosticKind::CannotDereferenceNonPointer(x_ty.inferred_type.to_string())
                .error_in(expr_span),
        )
    }
}
