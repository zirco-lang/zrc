//! type checking for unary operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::Expr;
use zrc_utils::span::{Span, Spannable};

use super::{
    super::{diagnostics::DiagnosticCollector, scope::Scope},
    helpers::{
        expect, expect_is_integer, expect_is_signed_integer, expr_to_place, handle_type_error,
    },
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck a unary not
pub fn type_expr_unary_not<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryNot(Box::new(x_ty)).in_span(expr_span),
        });
    }

    if let Err(diag) = expect(
        x_ty.inferred_type == TastType::Bool,
        "boolean".to_string(),
        x_ty.inferred_type.to_string(),
        expr_span,
    ) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryNot(Box::new(x_ty)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryNot(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a bit not
pub fn type_expr_unary_bitwise_not<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)).in_span(expr_span),
        });
    }

    if let Err(diag) = expect_is_integer(&x_ty.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryBitwiseNot(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a unary minus
pub fn type_expr_unary_minus<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryMinus(Box::new(x_ty)).in_span(expr_span),
        });
    }

    if let Err(diag) = expect_is_signed_integer(&x_ty.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryMinus(Box::new(x_ty)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: x_ty.inferred_type.clone(),
        kind: TypedExprKind::UnaryMinus(Box::new(x_ty)).in_span(expr_span),
    })
}

/// Typeck a unary ref
pub fn type_expr_unary_address_of<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        // expr_to_place can fail, so handle it
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryAddressOf(Box::new(match expr_to_place(expr_span, x_ty.clone()) {
                Ok(place) => place,
                Err(diag) => {
                    diagnostics.push(diag);
                    return Ok(TypedExpr {
                        inferred_type: TastType::Poison,
                        kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
                    });
                }
            }))
                .in_span(expr_span),
        });
    }

    let place = match expr_to_place(expr_span, x_ty.clone()) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    Ok(TypedExpr {
        inferred_type: TastType::Ptr(Box::new(x_ty.inferred_type.clone())),
        kind: TypedExprKind::UnaryAddressOf(Box::new(place))
            .in_span(expr_span),
    })
}

/// Typeck a unary deref
pub fn type_expr_unary_dereference<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryDereference(Box::new(x_ty)).in_span(expr_span),
        });
    }

    if let TastType::Ptr(tt) = x_ty.inferred_type.clone() {
        Ok(TypedExpr {
            inferred_type: *tt,
            kind: TypedExprKind::UnaryDereference(Box::new(x_ty)).in_span(expr_span),
        })
    } else {
        let diag = DiagnosticKind::CannotDereferenceNonPointer(x_ty.inferred_type.to_string())
            .error_in(expr_span);
        diagnostics.push(diag);
        Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::UnaryDereference(Box::new(x_ty)).in_span(expr_span),
        })
    }
}

/// Typeck a prefix increment (++x)
pub fn type_expr_prefix_increment<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PrefixIncrement(Box::new(match expr_to_place(expr_span, x_ty) {
                Ok(place) => place,
                Err(diag) => {
                    diagnostics.push(diag);
                    return Ok(TypedExpr {
                        inferred_type: TastType::Poison,
                        kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
                    });
                }
            }))
                .in_span(expr_span),
        });
    }

    let place = match expr_to_place(expr_span, x_ty) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    if let Err(diag) = expect_is_integer(&place.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PrefixIncrement(Box::new(place)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: place.inferred_type.clone(),
        kind: TypedExprKind::PrefixIncrement(Box::new(place)).in_span(expr_span),
    })
}

/// Typeck a prefix decrement (--x)
pub fn type_expr_prefix_decrement<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PrefixDecrement(Box::new(match expr_to_place(expr_span, x_ty) {
                Ok(place) => place,
                Err(diag) => {
                    diagnostics.push(diag);
                    return Ok(TypedExpr {
                        inferred_type: TastType::Poison,
                        kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
                    });
                }
            }))
                .in_span(expr_span),
        });
    }

    let place = match expr_to_place(expr_span, x_ty) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    if let Err(diag) = expect_is_integer(&place.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PrefixDecrement(Box::new(place)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: place.inferred_type.clone(),
        kind: TypedExprKind::PrefixDecrement(Box::new(place)).in_span(expr_span),
    })
}

/// Typeck a postfix increment (x++)
pub fn type_expr_postfix_increment<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PostfixIncrement(Box::new(match expr_to_place(expr_span, x_ty) {
                Ok(place) => place,
                Err(diag) => {
                    diagnostics.push(diag);
                    return Ok(TypedExpr {
                        inferred_type: TastType::Poison,
                        kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
                    });
                }
            }))
                .in_span(expr_span),
        });
    }

    let place = match expr_to_place(expr_span, x_ty) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    if let Err(diag) = expect_is_integer(&place.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PostfixIncrement(Box::new(place)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: place.inferred_type.clone(),
        kind: TypedExprKind::PostfixIncrement(Box::new(place)).in_span(expr_span),
    })
}

/// Typeck a postfix decrement (x--)
pub fn type_expr_postfix_decrement<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_span = x.0.span();
    let x_ty = handle_type_error(type_expr(scope, diagnostics, x), diagnostics, x_span);

    // If operand is poison, propagate poison
    if x_ty.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PostfixDecrement(Box::new(match expr_to_place(expr_span, x_ty) {
                Ok(place) => place,
                Err(diag) => {
                    diagnostics.push(diag);
                    return Ok(TypedExpr {
                        inferred_type: TastType::Poison,
                        kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
                    });
                }
            }))
                .in_span(expr_span),
        });
    }

    let place = match expr_to_place(expr_span, x_ty) {
        Ok(p) => p,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            });
        }
    };

    if let Err(diag) = expect_is_integer(&place.inferred_type, x_span) {
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::PostfixDecrement(Box::new(place)).in_span(expr_span),
        });
    }

    Ok(TypedExpr {
        inferred_type: place.inferred_type.clone(),
        kind: TypedExprKind::PostfixDecrement(Box::new(place)).in_span(expr_span),
    })
}
