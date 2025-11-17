//! type checking for the indexing and access operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Expr, ExprKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::{diagnostics::DiagnosticCollector, scope::Scope},
    helpers::{expr_to_place, handle_type_error, try_coerce_to},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck an index expr
pub fn type_expr_index<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    ptr: Expr<'input>,
    offset: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let ptr_span = ptr.0.span();
    let ptr_t = handle_type_error(type_expr(scope, diagnostics, ptr), diagnostics, ptr_span);
    let offset_span = offset.0.span();
    let offset_t = handle_type_error(type_expr(scope, diagnostics, offset), diagnostics, offset_span);

    // If either is poison, propagate poison
    if ptr_t.inferred_type.is_poison() || offset_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)).in_span(expr_span),
        });
    }

    // Allow {int} to implicitly convert to usize
    let offset_final = if offset_t.inferred_type == TastType::Usize {
        offset_t
    } else if offset_t
        .inferred_type
        .can_implicitly_cast_to(&TastType::Usize)
    {
        try_coerce_to(offset_t, &TastType::Usize)
    } else {
        let diag = DiagnosticKind::ExpectedGot {
            expected: "usize".to_string(),
            got: offset_t.inferred_type.to_string(),
        }
        .error_in(offset_t.kind.span());
        diagnostics.push(diag);
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)).in_span(expr_span),
        });
    };

    if let TastType::Ptr(points_to_ty) = ptr_t.inferred_type.clone() {
        Ok(TypedExpr {
            inferred_type: *points_to_ty,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_final)).in_span(expr_span),
        })
    } else {
        let diag = DiagnosticKind::CannotIndexIntoNonPointer(ptr_t.inferred_type.to_string())
            .error_in(expr_span);
        diagnostics.push(diag);
        Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_final)).in_span(expr_span),
        })
    }
}

/// Typeck a dot expr
pub fn type_expr_dot<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    obj: Expr<'input>,
    key: Spanned<&'input str>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let obj_span = obj.0.span();
    let obj_t = handle_type_error(type_expr(scope, diagnostics, obj), diagnostics, obj_span);

    // If object is poison, propagate poison
    if obj_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
        });
    }

    if let TastType::Struct(fields) | TastType::Union(fields) = obj_t.inferred_type.clone() {
        if let Some(ty) = fields.get(key.value()) {
            let place = match expr_to_place(obj_span, obj_t) {
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
                inferred_type: ty.clone(),
                kind: TypedExprKind::Dot(Box::new(place), key)
                    .in_span(expr_span),
            })
        } else {
            let diag = DiagnosticKind::StructOrUnionDoesNotHaveMember(
                obj_t.inferred_type.to_string(),
                key.into_value().to_string(),
            )
            .error_in(expr_span);
            diagnostics.push(diag);
            Ok(TypedExpr {
                inferred_type: TastType::Poison,
                kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
            })
        }
    } else {
        let diag = DiagnosticKind::StructMemberAccessOnNonStruct(obj_t.inferred_type.to_string())
            .error_in(expr_span);
        diagnostics.push(diag);
        Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
        })
    }
}

/// Typeck an arrow expr
pub fn type_expr_arrow<'input>(
    scope: &Scope<'input, '_>,
    diagnostics: &DiagnosticCollector,
    expr_span: Span,
    obj: Box<Expr<'input>>,
    key: Spanned<&'input str>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let obj_span = obj.0.span();
    let obj_t = handle_type_error(type_expr(scope, diagnostics, *obj.clone()), diagnostics, obj_span);

    // If object is poison, propagate poison
    if obj_t.inferred_type.is_poison() {
        return Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
        });
    }

    if let TastType::Ptr(_) = obj_t.inferred_type {
        type_expr(
            scope,
            diagnostics,
            Expr(Spanned::from_span_and_value(
                Span::from_positions_and_file(obj.0.start(), key.end(), obj.0.span().file_name()),
                ExprKind::Dot(
                    Box::new(Expr(
                        obj.0.span().containing(ExprKind::UnaryDereference(obj)),
                    )),
                    key,
                ),
            )),
        )
    } else {
        let diag = DiagnosticKind::CannotDereferenceNonPointer(obj_t.inferred_type.to_string())
            .error_in(expr_span);
        diagnostics.push(diag);
        Ok(TypedExpr {
            inferred_type: TastType::Poison,
            kind: TypedExprKind::Identifier("__poison__").in_span(expr_span),
        })
    }
}
    }
}
