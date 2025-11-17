//! type checking for binary operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Expr, Logical};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{
        expect, expect_is_integer, expect_is_unsigned_integer, resolve_binary_int_operands,
        try_coerce_to,
    },
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck a logical expr
pub fn type_expr_logical<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    op: Logical,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_span = lhs.0.span();
    let lhs_t = handle_type_error(type_expr(scope, diagnostics, lhs), diagnostics, lhs_span);
    let rhs_span = rhs.0.span();
    let rhs_t = handle_type_error(type_expr(scope, diagnostics, rhs), diagnostics, rhs_span);

    // If either operand is poison, propagate poison
    if lhs_t.inferred_type.is_poison() || rhs_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
        });
    }

    if let Err(diag) = expect(
        lhs_t.inferred_type == TastType::Bool,
        "bool".to_string(),
        lhs_t.inferred_type.to_string(),
        lhs_span,
    ) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
        });
    }
    
    if let Err(diag) = expect(
        rhs_t.inferred_type == TastType::Bool,
        "bool".to_string(),
        rhs_t.inferred_type.to_string(),
        rhs_span,
    ) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Logical(op, Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
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

    // If either operand is poison, propagate poison
    if lhs_t.inferred_type.is_poison() || rhs_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Equality(op, Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
        });
    }

    let (final_lhs, final_rhs) =
        if lhs_t.inferred_type.is_integer() && rhs_t.inferred_type.is_integer() {
            let (_, resolved_lhs, resolved_rhs) = resolve_binary_int_operands(lhs_t, rhs_t);

            // Check if types match after resolution
            if resolved_lhs.inferred_type == resolved_rhs.inferred_type {
                (resolved_lhs, resolved_rhs)
            } else {
                return Err(DiagnosticKind::EqualityOperators(
                    resolved_lhs.inferred_type.to_string(),
                    resolved_rhs.inferred_type.to_string(),
                )
                .error_in(expr_span));
            }
        } else if let (TastType::Ptr(_), TastType::Ptr(_)) =
            (&lhs_t.inferred_type, &rhs_t.inferred_type)
        {
            // *T == *U is valid
            (lhs_t, rhs_t)
        } else if lhs_t.inferred_type == TastType::Bool && rhs_t.inferred_type == TastType::Bool {
            // bool == bool is valid
            (lhs_t, rhs_t)
        } else {
            return Err(DiagnosticKind::EqualityOperators(
                lhs_t.inferred_type.to_string(),
                rhs_t.inferred_type.to_string(),
            )
            .error_in(expr_span));
        };

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Equality(op, Box::new(final_lhs), Box::new(final_rhs))
            .in_span(expr_span),
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

    // If either operand is poison, propagate poison
    if lhs_t.inferred_type.is_poison() || rhs_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::BinaryBitwise(op, Box::new(lhs_t), Box::new(rhs_t))
                .in_span(expr_span),
        });
    }

    expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
    expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

    if matches!(op, BinaryBitwise::Shl | BinaryBitwise::Shr) {
        // we can only shift by an unsigned integer
        expect_is_unsigned_integer(&rhs_t.inferred_type, rhs_span)?;
        Ok(TypedExpr {
            inferred_type: lhs_t.inferred_type.clone(),
            kind: TypedExprKind::BinaryBitwise(op, Box::new(lhs_t), Box::new(rhs_t))
                .in_span(expr_span),
        })
    } else {
        // otherwise these must be the same type (with {int} support)
        let (result_type, final_lhs, final_rhs) = resolve_binary_int_operands(lhs_t, rhs_t);

        // Check if types match after resolution
        if final_lhs.inferred_type != final_rhs.inferred_type {
            return Err(DiagnosticKind::ExpectedSameType(
                final_lhs.inferred_type.to_string(),
                final_rhs.inferred_type.to_string(),
            )
            .error_in(expr_span));
        }

        Ok(TypedExpr {
            inferred_type: result_type,
            kind: TypedExprKind::BinaryBitwise(op, Box::new(final_lhs), Box::new(final_rhs))
                .in_span(expr_span),
        })
    }
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

    // If either operand is poison, propagate poison
    if lhs_t.inferred_type.is_poison() || rhs_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Comparison(op, Box::new(lhs_t), Box::new(rhs_t))
                .in_span(expr_span),
        });
    }

    expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
    expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

    // Handle {int} type resolution
    let (_, final_lhs, final_rhs) = resolve_binary_int_operands(lhs_t, rhs_t);

    // Check if types match after resolution
    if final_lhs.inferred_type != final_rhs.inferred_type {
        return Err(DiagnosticKind::ExpectedSameType(
            final_lhs.inferred_type.to_string(),
            final_rhs.inferred_type.to_string(),
        )
        .error_in(expr_span));
    }

    Ok(TypedExpr {
        inferred_type: TastType::Bool,
        kind: TypedExprKind::Comparison(op, Box::new(final_lhs), Box::new(final_rhs))
            .in_span(expr_span),
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

    // If either operand is poison, propagate poison
    if lhs_t.inferred_type.is_poison() || rhs_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Arithmetic(op, Box::new(lhs_t), Box::new(rhs_t))
                .in_span(expr_span),
        });
    }

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

        // For pointer arithmetic, rhs should be usize or {int} (which implicitly
        // converts to usize)
        let final_rhs = if rhs_t.inferred_type == TastType::Usize {
            rhs_t
        } else if rhs_t.inferred_type.can_implicitly_cast_to(&TastType::Usize) {
            try_coerce_to(rhs_t, &TastType::Usize)
        } else {
            return Err(DiagnosticKind::ExpectedGot {
                expected: "usize".to_string(),
                got: rhs_t.inferred_type.to_string(),
            }
            .error_in(rhs_t.kind.span()));
        };

        Ok(TypedExpr {
            inferred_type: lhs_t.inferred_type.clone(),
            kind: TypedExprKind::Arithmetic(op, Box::new(lhs_t), Box::new(final_rhs))
                .in_span(expr_span),
        })
    } else {
        expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
        expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

        // Handle {int} type resolution
        let (result_type, final_lhs, final_rhs) = resolve_binary_int_operands(lhs_t, rhs_t);

        // Check if types match after resolution
        if final_lhs.inferred_type != final_rhs.inferred_type {
            return Err(DiagnosticKind::ExpectedSameType(
                final_lhs.inferred_type.to_string(),
                final_rhs.inferred_type.to_string(),
            )
            .error_in(expr_span));
        }

        Ok(TypedExpr {
            inferred_type: result_type,
            kind: TypedExprKind::Arithmetic(op, Box::new(final_lhs), Box::new(final_rhs))
                .in_span(expr_span),
        })
    }
}
