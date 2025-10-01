//! type checking for misc expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::{expr::Expr, ty::Type};
use zrc_utils::span::{Span, Spannable};

use super::{super::scope::Scope, helpers::expect, type_expr};
use crate::{
    tast::{
        expr::{TypedExpr, TypedExprKind},
        ty::Type as TastType,
    },
    typeck::resolve_type,
};

/// Typeck a comma expr
pub fn type_expr_comma<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    lhs: Expr<'input>,
    rhs: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let lhs_t = type_expr(scope, lhs)?;
    let rhs_t = type_expr(scope, rhs)?;
    Ok(TypedExpr {
        inferred_type: rhs_t.inferred_type.clone(),
        kind: TypedExprKind::Comma(Box::new(lhs_t), Box::new(rhs_t)).in_span(expr_span),
    })
}

/// Typeck a ternary expr
pub fn type_expr_ternary<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    cond: Expr<'input>,
    if_true: Expr<'input>,
    if_false: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let cond_span = cond.0.span();
    let cond_t = type_expr(scope, cond)?;
    let if_true_t = type_expr(scope, if_true)?;
    let if_false_t = type_expr(scope, if_false)?;

    expect(
        cond_t.inferred_type == TastType::Bool,
        "boolean".to_string(),
        cond_t.inferred_type.to_string(),
        cond_span,
    )?;

    // Handle {int} type resolution in ternary branches
    let (result_type, if_true_final, if_false_final) =
        if if_true_t.inferred_type == if_false_t.inferred_type {
            // Both branches have the same type
            if matches!(if_true_t.inferred_type, TastType::Int) {
                // Both are {int}, resolve to i32
                let if_true_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: if_true_t.kind,
                };
                let if_false_resolved = TypedExpr {
                    inferred_type: TastType::I32,
                    kind: if_false_t.kind,
                };
                (TastType::I32, if_true_resolved, if_false_resolved)
            } else {
                (if_true_t.inferred_type.clone(), if_true_t, if_false_t)
            }
        } else if matches!(if_true_t.inferred_type, TastType::Int)
            && if_true_t
                .inferred_type
                .can_implicitly_cast_to(&if_false_t.inferred_type)
        {
            // if_true is {int}, resolve to if_false type
            let if_true_with_resolved_type = TypedExpr {
                inferred_type: if_false_t.inferred_type.clone(),
                kind: if_true_t.kind,
            };
            (
                if_false_t.inferred_type.clone(),
                if_true_with_resolved_type,
                if_false_t,
            )
        } else if matches!(if_false_t.inferred_type, TastType::Int)
            && if_false_t
                .inferred_type
                .can_implicitly_cast_to(&if_true_t.inferred_type)
        {
            // if_false is {int}, resolve to if_true type
            let if_false_with_resolved_type = TypedExpr {
                inferred_type: if_true_t.inferred_type.clone(),
                kind: if_false_t.kind,
            };
            (
                if_true_t.inferred_type.clone(),
                if_true_t,
                if_false_with_resolved_type,
            )
        } else {
            // Types don't match and can't be implicitly cast
            return Err(DiagnosticKind::ExpectedSameType(
                if_true_t.inferred_type.to_string(),
                if_false_t.inferred_type.to_string(),
            )
            .error_in(expr_span));
        };

    Ok(TypedExpr {
        inferred_type: result_type,
        kind: TypedExprKind::Ternary(
            Box::new(cond_t),
            Box::new(if_true_final),
            Box::new(if_false_final),
        )
        .in_span(expr_span),
    })
}

/// Typeck a cast expr
pub fn type_expr_cast<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
    ty: Type<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let x_t = type_expr(scope, x)?;
    let ty_span = ty.0.span();
    let resolved_ty = resolve_type(scope.types, ty)?;

    // Handle {int} type resolution
    if matches!(x_t.inferred_type, TastType::Int) {
        if resolved_ty.is_integer() {
            // {int} -> integer cast is just a type resolution, no runtime operation needed
            // Preserve the original cast expression span
            return Ok(TypedExpr {
                inferred_type: resolved_ty,
                kind: x_t.kind.into_value().in_span(expr_span),
            });
        }
        // {int} -> non-integer cast (like *T): resolve {int} to i32 first,
        // then apply the cast
        let x_resolved = TypedExpr {
            inferred_type: TastType::I32,
            kind: x_t.kind,
        };
        return Ok(TypedExpr {
            inferred_type: resolved_ty.clone(),
            kind: TypedExprKind::Cast(Box::new(x_resolved), resolved_ty.in_span(ty_span))
                .in_span(expr_span),
        });
    }

    if x_t.inferred_type.is_integer() && resolved_ty.is_integer() {
        // int -> int cast is valid
    } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (&x_t.inferred_type, &resolved_ty) {
        // *T -> *U cast is valid
    } else if let (TastType::Ptr(_), _) | (_, TastType::Ptr(_)) = (&x_t.inferred_type, &resolved_ty)
    {
        // ensure one is an int
        if x_t.inferred_type.is_integer() || resolved_ty.is_integer() {
            // *T -> int or int -> *T cast is valid
        } else {
            return Err(DiagnosticKind::InvalidCast(
                x_t.inferred_type.to_string(),
                resolved_ty.to_string(),
            )
            .error_in(expr_span));
        }
    } else if x_t.inferred_type == TastType::Bool && resolved_ty.is_integer() {
        // bool -> int cast is valid
    } else {
        return Err(DiagnosticKind::InvalidCast(
            x_t.inferred_type.to_string(),
            resolved_ty.to_string(),
        )
        .error_in(expr_span));
    }

    Ok(TypedExpr {
        inferred_type: resolved_ty.clone(),
        kind: TypedExprKind::Cast(Box::new(x_t), resolved_ty.in_span(ty_span)).in_span(expr_span),
    })
}

/// Typeck a sizeof T expr
pub fn type_expr_size_of_type<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    ty: Type<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let resolved_ty = resolve_type(scope.types, ty)?;
    Ok(TypedExpr {
        inferred_type: TastType::Usize,
        kind: TypedExprKind::SizeOf(resolved_ty).in_span(expr_span),
    })
}

/// Typeck a sizeof(T) expr
pub fn type_expr_size_of_expr<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    x: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    // resolve `sizeof(expr)` by finding `typeof expr` and then basically becoming
    // "sizeof typeof expr"
    let x_ty = type_expr(scope, x)?;
    Ok(TypedExpr {
        inferred_type: TastType::Usize,
        kind: TypedExprKind::SizeOf(x_ty.inferred_type).in_span(expr_span),
    })
}

#[cfg(test)]
mod tests {
    use zrc_parser::{ast::expr::Expr, lexer::NumberLiteral};
    use zrc_utils::{
        span::{Span, Spannable},
        spanned,
    };

    use super::*;
    use crate::{
        tast::{
            expr::{TypedExpr, TypedExprKind},
            ty::Type as TastType,
        },
        typeck::scope::GlobalScope,
    };

    #[test]
    fn sizeof_expr_works_as_expected() {
        assert_eq!(
            type_expr(
                &GlobalScope::new().create_subscope(),
                Expr::build_sizeof_expr(
                    Span::from_positions(0, 9),
                    Expr::build_number(spanned!(8, NumberLiteral::Decimal("1"), 9), None)
                ),
            ),
            Ok(TypedExpr {
                inferred_type: TastType::Usize,
                kind: TypedExprKind::SizeOf(TastType::Int).in_span(Span::from_positions(0, 9)),
            })
        );
    }
}
