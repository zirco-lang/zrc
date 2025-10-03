//! type checking for the assignment operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Assignment, Expr};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{desugar_assignment, expr_to_place},
    type_expr,
};
use crate::tast::expr::{TypedExpr, TypedExprKind};

/// Typeck and desugar an assignment expr
pub fn type_expr_assignment<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    mode: Assignment,
    place: Expr<'input>,
    value: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    // Desugar `x += y` to `x = x + y`.
    let (place, value) = desugar_assignment(mode, place, value);

    let place_t = expr_to_place(expr_span, type_expr(scope, place)?)?;
    let value_t = type_expr(scope, value)?;

    if place_t.inferred_type != value_t.inferred_type {
        return Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
            expected: place_t.inferred_type.to_string(),
            got: value_t.inferred_type.to_string(),
        }
        .error_in(expr_span));
    }

    Ok(TypedExpr {
        inferred_type: place_t.inferred_type.clone(),
        kind: TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t))
            .in_span_no_file(expr_span),
    })
}
