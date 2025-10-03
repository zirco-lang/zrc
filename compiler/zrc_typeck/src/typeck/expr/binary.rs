//! type checking for binary operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Expr, Logical};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{expect, expect_identical_types, expect_is_integer, expect_is_unsigned_integer},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck a logical expr
pub fn type_expr_logical<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    op: Logical,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_span = lhs.0.span();
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_span = rhs.0.span();
    let rhs_t = type_expr(scope, rhs)?;

    expect(
        lhs_t.inferred_type == TastType::Bool,
        "bool".to_string(),
        lhs_t.inferred_type.to_string(),
        lhs_span,
    )?;
    expect(
        rhs_t.inferred_type == TastType::Bool,
        "bool".to_string(),
        rhs_t.inferred_type.to_string(),
        rhs_span,
    )?;

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t))
            .in_span_no_file(expr_span),
    })
}

/// Typeck an eq expr
pub fn type_expr_equality<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    op: Equality,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_t = type_expr(scope, rhs)?;

    if lhs_t.inferred_type.is_integer()
        && rhs_t.inferred_type.is_integer()
        && lhs_t.inferred_type == rhs_t.inferred_type
    {
        // int == int is valid
    } else if let (TastType::Ptr(_), TastType::Ptr(_)) =
        (&lhs_t.inferred_type, &rhs_t.inferred_type)
    {
        // *T == *U is valid
    } else if lhs_t.inferred_type == TastType::Bool && rhs_t.inferred_type == TastType::Bool {
        // bool == bool is valid
    } else {
        return Err(DiagnosticKind::EqualityOperators(
            lhs_t.inferred_type.to_string(),
            rhs_t.inferred_type.to_string(),
        )
        .error_in(expr_span));
    }

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Equality(op, Box::new(lhs_t), Box::new(rhs_t))
            .in_span_no_file(expr_span),
    })
}

/// Typeck a bitwise expr
pub fn type_expr_binary_bitwise<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    op: BinaryBitwise,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_span = lhs.0.span();
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_span = rhs.0.span();
    let rhs_t = type_expr(scope, rhs)?;

    expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
    expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

    if matches!(op, BinaryBitwise::Shl | BinaryBitwise::Shr) {
        // we can only shift by an unsigned integer
        expect_is_unsigned_integer(&rhs_t.inferred_type, rhs_span)?;
    } else {
        // otherwise these must be the same type
        expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;
    }

    Ok(TypedExpr {
        inferred_type: lhs_t.inferred_type.clone(),
        kind: TypedExprKind::BinaryBitwise(op, Box::new(lhs_t), Box::new(rhs_t))
            .in_span_no_file(expr_span),
    })
}

/// Typeck a cmp expr
pub fn type_expr_comparison<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    op: Comparison,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_span = lhs.0.span();
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_span = rhs.0.span();
    let rhs_t = type_expr(scope, rhs)?;

    expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
    expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

    expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Comparison(op, Box::new(lhs_t), Box::new(rhs_t))
            .in_span_no_file(expr_span),
    })
}

/// Typeck an arithmetic expr
pub fn type_expr_arithmetic<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    op: Arithmetic,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_span = lhs.0.span();
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_span = rhs.0.span();
    let rhs_t = type_expr(scope, rhs)?;

    if let TastType::Ptr(_) = lhs_t.inferred_type {
        if matches!(
            op,
            Arithmetic::Division | Arithmetic::Multiplication | Arithmetic::Modulo
        ) {
            return Err(
                DiagnosticKind::InvalidPointerArithmeticOperation(op.to_string())
                    .error_in(lhs_span),
            );
        }

        expect(
            rhs_t.inferred_type == TastType::Usize,
            "usize".to_string(),
            rhs_t.inferred_type.to_string(),
            rhs_t.kind.span(),
        )?;
    } else {
        expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
        expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

        expect_identical_types(&lhs_t.inferred_type, &rhs_t.inferred_type, expr_span)?;
    }

    Ok(TypedExpr {
        inferred_type: lhs_t.inferred_type.clone(),
        kind: TypedExprKind::Arithmetic(op, Box::new(lhs_t), Box::new(rhs_t))
            .in_span_no_file(expr_span),
    })
}
