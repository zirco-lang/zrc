//! type checking for binary operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Expr, Logical};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{expect, expect_is_integer, expect_is_unsigned_integer},
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
    let _lhs_span = lhs.0.span();
    let lhs_t = type_expr(scope, lhs)?;
    let _rhs_span = rhs.0.span();
    let rhs_t = type_expr(scope, rhs)?;

    let (final_lhs, final_rhs) =
        if lhs_t.inferred_type.is_integer() && rhs_t.inferred_type.is_integer() {
            if lhs_t.inferred_type == rhs_t.inferred_type {
                // int == int is valid (same type)
                if matches!(lhs_t.inferred_type, TastType::Int) {
                    // Both are {int}, resolve to i32
                    let lhs_resolved = TypedExpr {
                        inferred_type: TastType::I32,
                        kind: lhs_t.kind,
                    };
                    let rhs_resolved = TypedExpr {
                        inferred_type: TastType::I32,
                        kind: rhs_t.kind,
                    };
                    (lhs_resolved, rhs_resolved)
                } else {
                    (lhs_t, rhs_t)
                }
            } else if matches!(lhs_t.inferred_type, TastType::Int)
                && lhs_t
                    .inferred_type
                    .can_implicitly_cast_to(&rhs_t.inferred_type)
            {
                // lhs is {int}, resolve to rhs type
                let lhs_with_resolved_type = TypedExpr {
                    inferred_type: rhs_t.inferred_type.clone(),
                    kind: lhs_t.kind,
                };
                (lhs_with_resolved_type, rhs_t)
            } else if matches!(rhs_t.inferred_type, TastType::Int)
                && rhs_t
                    .inferred_type
                    .can_implicitly_cast_to(&lhs_t.inferred_type)
            {
                // rhs is {int}, resolve to lhs type
                let rhs_with_resolved_type = TypedExpr {
                    inferred_type: lhs_t.inferred_type.clone(),
                    kind: rhs_t.kind,
                };
                (lhs_t, rhs_with_resolved_type)
            } else {
                return Err(DiagnosticKind::EqualityOperators(
                    lhs_t.inferred_type.to_string(),
                    rhs_t.inferred_type.to_string(),
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
        let (result_type, final_lhs, final_rhs) = if lhs_t.inferred_type == rhs_t.inferred_type {
            // Both have the same type
            if matches!(lhs_t.inferred_type, TastType::Int) {
                // Both are {int}, resolve to i32
                let lhs_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: lhs_t.kind,
                };
                let rhs_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: rhs_t.kind,
                };
                (TastType::I32, lhs_resolved, rhs_resolved)
            } else {
                (lhs_t.inferred_type.clone(), lhs_t, rhs_t)
            }
        } else if matches!(lhs_t.inferred_type, TastType::Int)
            && lhs_t
                .inferred_type
                .can_implicitly_cast_to(&rhs_t.inferred_type)
        {
            // lhs is {int}, resolve to rhs type
            let lhs_with_resolved_type = TypedExpr {
                inferred_type: rhs_t.inferred_type.clone(),
                kind: lhs_t.kind,
            };
            (rhs_t.inferred_type.clone(), lhs_with_resolved_type, rhs_t)
        } else if matches!(rhs_t.inferred_type, TastType::Int)
            && rhs_t
                .inferred_type
                .can_implicitly_cast_to(&lhs_t.inferred_type)
        {
            // rhs is {int}, resolve to lhs type
            let rhs_with_resolved_type = TypedExpr {
                inferred_type: lhs_t.inferred_type.clone(),
                kind: rhs_t.kind,
            };
            (lhs_t.inferred_type.clone(), lhs_t, rhs_with_resolved_type)
        } else {
            // Types don't match
            return Err(DiagnosticKind::ExpectedSameType(
                lhs_t.inferred_type.to_string(),
                rhs_t.inferred_type.to_string(),
            )
            .error_in(expr_span));
        };

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

    expect_is_integer(&lhs_t.inferred_type, lhs_span)?;
    expect_is_integer(&rhs_t.inferred_type, rhs_span)?;

    // Handle {int} type resolution
    let (final_lhs, final_rhs) = if lhs_t.inferred_type == rhs_t.inferred_type {
        // Both have the same type
        if matches!(lhs_t.inferred_type, TastType::Int) {
            // Both are {int}, resolve to i32
            let lhs_resolved = TypedExpr {
                inferred_type: TastType::I32,
                kind: lhs_t.kind,
            };
            let rhs_resolved = TypedExpr {
                inferred_type: TastType::I32,
                kind: rhs_t.kind,
            };
            (lhs_resolved, rhs_resolved)
        } else {
            (lhs_t, rhs_t)
        }
    } else if matches!(lhs_t.inferred_type, TastType::Int)
        && lhs_t
            .inferred_type
            .can_implicitly_cast_to(&rhs_t.inferred_type)
    {
        // lhs is {int}, resolve to rhs type
        let lhs_with_resolved_type = TypedExpr {
            inferred_type: rhs_t.inferred_type.clone(),
            kind: lhs_t.kind,
        };
        (lhs_with_resolved_type, rhs_t)
    } else if matches!(rhs_t.inferred_type, TastType::Int)
        && rhs_t
            .inferred_type
            .can_implicitly_cast_to(&lhs_t.inferred_type)
    {
        // rhs is {int}, resolve to lhs type
        let rhs_with_resolved_type = TypedExpr {
            inferred_type: lhs_t.inferred_type.clone(),
            kind: rhs_t.kind,
        };
        (lhs_t, rhs_with_resolved_type)
    } else {
        // Types don't match
        return Err(DiagnosticKind::ExpectedSameType(
            lhs_t.inferred_type.to_string(),
            rhs_t.inferred_type.to_string(),
        )
        .error_in(expr_span));
    };

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
        } else if matches!(rhs_t.inferred_type, TastType::Int)
            && rhs_t.inferred_type.can_implicitly_cast_to(&TastType::Usize)
        {
            // rhs is {int}, resolve to usize
            TypedExpr {
                inferred_type: TastType::Usize,
                kind: rhs_t.kind,
            }
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
        let (result_type, final_lhs, final_rhs) = if lhs_t.inferred_type == rhs_t.inferred_type {
            // Both have the same type
            if matches!(lhs_t.inferred_type, TastType::Int) {
                // Both are {int}, resolve to i32
                let lhs_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: lhs_t.kind,
                };
                let rhs_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: rhs_t.kind,
                };
                (TastType::I32, lhs_resolved, rhs_resolved)
            } else {
                (lhs_t.inferred_type.clone(), lhs_t, rhs_t)
            }
        } else if matches!(lhs_t.inferred_type, TastType::Int)
            && lhs_t
                .inferred_type
                .can_implicitly_cast_to(&rhs_t.inferred_type)
        {
            // lhs is {int}, resolve to rhs type
            let lhs_with_resolved_type = TypedExpr {
                inferred_type: rhs_t.inferred_type.clone(),
                kind: lhs_t.kind,
            };
            (rhs_t.inferred_type.clone(), lhs_with_resolved_type, rhs_t)
        } else if matches!(rhs_t.inferred_type, TastType::Int)
            && rhs_t
                .inferred_type
                .can_implicitly_cast_to(&lhs_t.inferred_type)
        {
            // rhs is {int}, resolve to lhs type
            let rhs_with_resolved_type = TypedExpr {
                inferred_type: lhs_t.inferred_type.clone(),
                kind: rhs_t.kind,
            };
            (lhs_t.inferred_type.clone(), lhs_t, rhs_with_resolved_type)
        } else {
            // Types don't match
            return Err(DiagnosticKind::ExpectedSameType(
                lhs_t.inferred_type.to_string(),
                rhs_t.inferred_type.to_string(),
            )
            .error_in(expr_span));
        };

        Ok(TypedExpr {
            inferred_type: result_type,
            kind: TypedExprKind::Arithmetic(op, Box::new(final_lhs), Box::new(final_rhs))
                .in_span(expr_span),
        })
    }
}
