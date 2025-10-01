//! type checking for the indexing and access operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Expr, ExprKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::scope::Scope,
    helpers::{expect, expr_to_place},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck an index expr
pub fn type_expr_index<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    ptr: Expr<'input>,
    offset: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let ptr_t = type_expr(scope, ptr)?;
    let offset_t = type_expr(scope, offset)?;

    expect(
        offset_t.inferred_type == TastType::Usize,
        "usize".to_string(),
        offset_t.inferred_type.to_string(),
        offset_t.kind.span(),
    )?;

    if let TastType::Ptr(points_to_ty) = ptr_t.inferred_type.clone() {
        Ok(TypedExpr {
            inferred_type: *points_to_ty,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)).in_span(expr_span),
        })
    } else {
        Err(
            DiagnosticKind::CannotIndexIntoNonPointer(ptr_t.inferred_type.to_string())
                .error_in(expr_span),
        )
    }
}

/// Typeck a dot expr
pub fn type_expr_dot<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    obj: Expr<'input>,
    key: Spanned<&'input str>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let obj_span = obj.0.span();
    let obj_t = type_expr(scope, obj)?;

    if let TastType::Struct(fields) | TastType::Union(fields) = obj_t.inferred_type.clone() {
        if let Some(ty) = fields.get(key.value()) {
            Ok(TypedExpr {
                inferred_type: ty.clone(),
                kind: TypedExprKind::Dot(Box::new(expr_to_place(obj_span, obj_t)?), key)
                    .in_span(expr_span),
            })
        } else {
            Err(DiagnosticKind::StructOrUnionDoesNotHaveMember(
                obj_t.inferred_type.to_string(),
                key.into_value().to_string(),
            )
            .error_in(expr_span))
        }
    } else {
        Err(
            DiagnosticKind::StructMemberAccessOnNonStruct(obj_t.inferred_type.to_string())
                .error_in(expr_span),
        )
    }
}

/// Typeck an arrow expr
pub fn type_expr_arrow<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    obj: Box<Expr<'input>>,
    key: Spanned<&'input str>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let obj_t = type_expr(scope, *obj.clone())?;

    if let TastType::Ptr(_) = obj_t.inferred_type {
        type_expr(
            scope,
            Expr(Spanned::from_span_and_value(
                Span::from_positions(obj.0.start(), key.end()),
                ExprKind::Dot(
                    Box::new(Expr(
                        obj.0.span().containing(ExprKind::UnaryDereference(obj)),
                    )),
                    key,
                ),
            )),
        )
    } else {
        Err(
            DiagnosticKind::CannotDereferenceNonPointer(obj_t.inferred_type.to_string())
                .error_in(expr_span),
        )
    }
}
