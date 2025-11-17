//! type checking for the assignment operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Assignment, Expr};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::{diagnostics::DiagnosticCollector, scope::Scope},
    helpers::{desugar_assignment, expr_to_place, handle_type_error, try_coerce_to},
    type_expr,
};
use crate::tast::{expr::{TypedExpr, TypedExprKind}, ty::Type as TastType};

/// Typeck and desugar an assignment expr
pub fn type_expr_assignment<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    mode: Assignment,
    place: Expr<'input>,
    value: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    // Desugar `x += y` to `x = x + y`.
    let (place, value) = desugar_assignment(mode, place, value);

    let place_span = place.0.span();
    let place_expr = handle_type_error(type_expr(scope, diagnostics, place), diagnostics, place_span);
    let value_span = value.0.span();
    let value_t = handle_type_error(type_expr(scope, diagnostics, value), diagnostics, value_span);

    // If either is poison, propagate poison
    if place_expr.inferred_type.is_poison() || value_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Assignment(Box::new(place_expr), Box::new(value_t))
                .in_span(expr_span),
        });
    }

    let place_t = match expr_to_place(expr_span, place_expr) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    if place_t.inferred_type == value_t.inferred_type {
        Ok(TypedExpr {
            inferred_type: place_t.inferred_type.clone(),
            kind: TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t))
                .in_span(expr_span),
        })
    } else if value_t
        .inferred_type
        .can_implicitly_cast_to(&place_t.inferred_type)
    {
        // Try to coerce the value to the place type
        let value_coerced = try_coerce_to(value_t, &place_t.inferred_type);
        Ok(TypedExpr {
            inferred_type: place_t.inferred_type.clone(),
            kind: TypedExprKind::Assignment(Box::new(place_t), Box::new(value_coerced))
                .in_span(expr_span),
        })
    } else {
        let diag = DiagnosticKind::InvalidAssignmentRightHandSideType {
            expected: place_t.inferred_type.to_string(),
            got: value_t.inferred_type.to_string(),
        }
        .error_in(expr_span);
        diagnostics.push(diag);
        Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t))
                .in_span(expr_span),
        })
    }
}
