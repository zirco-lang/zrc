//! type checking for the indexing and access operators

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::expr::{Expr, ExprKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::scope::Scope,
    helpers::{expr_to_place, try_coerce_to},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    ty::Type as TastType,
};

/// Typeck an index expr
pub fn type_expr_index<'input>(
    scope: &mut Scope<'input, '_>,
    expr_span: Span,
    ptr: Expr<'input>,
    offset: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let ptr_t = type_expr(scope, ptr)?;
    let offset_t = type_expr(scope, offset)?;

    // Allow {int} to implicitly convert to usize
    let offset_final = if offset_t.inferred_type == TastType::Usize {
        offset_t
    } else if offset_t
        .inferred_type
        .can_implicitly_cast_to(&TastType::Usize)
    {
        try_coerce_to(offset_t, &TastType::Usize)
    } else {
        return Err(DiagnosticKind::ExpectedGot {
            expected: "usize".to_string(),
            got: offset_t.inferred_type.to_string(),
        }
        .error_in(offset_t.kind.span()));
    };

    if let TastType::Ptr(points_to_ty) = ptr_t.inferred_type.clone() {
        Ok(TypedExpr {
            inferred_type: *points_to_ty,
            kind: TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_final)).in_span(expr_span),
        })
    } else if let TastType::Array { element_type, .. } = ptr_t.inferred_type.clone() {
        // Arrays decay to pointers when indexed
        // Convert the array to a pointer to its first element
        let place = expr_to_place(scope, expr_span, ptr_t)?;
        let array_ptr_expr = TypedExpr {
            inferred_type: TastType::Ptr(element_type.clone()),
            kind: TypedExprKind::UnaryAddressOf(Box::new(place)).in_span(expr_span),
        };

        Ok(TypedExpr {
            inferred_type: *element_type,
            kind: TypedExprKind::Index(Box::new(array_ptr_expr), Box::new(offset_final))
                .in_span(expr_span),
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
    scope: &mut Scope<'input, '_>,
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
                kind: TypedExprKind::Dot(Box::new(expr_to_place(scope, obj_span, obj_t)?), key)
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
    scope: &mut Scope<'input, '_>,
    expr_span: Span,
    obj: Box<Expr<'input>>,
    key: Spanned<&'input str>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let obj_t = type_expr(scope, *obj.clone())?;

    if let TastType::Ptr(_) = obj_t.inferred_type {
        type_expr(
            scope,
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
        Err(
            DiagnosticKind::CannotDereferenceNonPointer(obj_t.inferred_type.to_string())
                .error_in(expr_span),
        )
    }
}
