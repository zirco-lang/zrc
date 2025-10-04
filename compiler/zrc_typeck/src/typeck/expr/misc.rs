//! type checking for misc expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::{expr::Expr, ty::Type};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{expect, expect_identical_types},
    type_expr,
};
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

    expect_identical_types(
        &if_true_t.inferred_type,
        &if_false_t.inferred_type,
        expr_span,
    )?;

    Ok(TypedExpr {
        inferred_type: if_true_t.inferred_type.clone(),
        kind: TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t))
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

/// Typeck a struct construction expr
#[allow(clippy::type_complexity)]
pub fn type_expr_struct_construction<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    ty: Type<'input>,
    fields: &zrc_utils::span::Spanned<
        Vec<zrc_utils::span::Spanned<(zrc_utils::span::Spanned<&'input str>, Expr<'input>)>>,
    >,
) -> Result<TypedExpr<'input>, Diagnostic> {
    use indexmap::IndexMap;

    // Resolve the type being constructed
    let resolved_ty = resolve_type(scope.types, ty)?;

    // Ensure it's a struct or union type
    let expected_fields = match &resolved_ty {
        TastType::Struct(fields) | TastType::Union(fields) => fields.clone(),
        TastType::I8
        | TastType::U8
        | TastType::I16
        | TastType::U16
        | TastType::I32
        | TastType::U32
        | TastType::I64
        | TastType::U64
        | TastType::Usize
        | TastType::Isize
        | TastType::Bool
        | TastType::Ptr(_)
        | TastType::Fn(_)
        | TastType::Opaque(_) => {
            return Err(DiagnosticKind::ExpectedGot {
                expected: "struct or union type".to_string(),
                got: resolved_ty.to_string(),
            }
            .error_in(expr_span));
        }
    };

    // Type check each field initialization
    let mut initialized_fields: IndexMap<&'input str, TypedExpr<'input>> = IndexMap::new();

    for field_init in fields.value() {
        let (field_name, field_expr) = field_init.value();
        let field_name_str = field_name.value();

        // Check if field exists in the struct
        let expected_type = expected_fields.get(field_name_str).ok_or_else(|| {
            DiagnosticKind::StructOrUnionDoesNotHaveMember(
                resolved_ty.to_string(),
                (*field_name_str).to_string(),
            )
            .error_in(field_name.span())
        })?;

        // Type check the field value
        let typed_field_expr = type_expr(scope, field_expr.clone())?;

        // Ensure the types match
        expect(
            &typed_field_expr.inferred_type == expected_type,
            expected_type.to_string(),
            typed_field_expr.inferred_type.to_string(),
            typed_field_expr.kind.span(),
        )?;

        // Check for duplicate field initialization
        if initialized_fields.contains_key(field_name_str) {
            return Err(
                DiagnosticKind::DuplicateStructMember((*field_name_str).to_string())
                    .error_in(field_name.span()),
            );
        }

        initialized_fields.insert(field_name_str, typed_field_expr);
    }

    // For structs (not unions), verify all fields are initialized
    if matches!(resolved_ty, TastType::Struct(_)) {
        for (field_name, _field_type) in &expected_fields {
            if !initialized_fields.contains_key(field_name) {
                return Err(DiagnosticKind::ExpectedGot {
                    expected: format!("initialization of field '{field_name}'"),
                    got: "missing field".to_string(),
                }
                .error_in(fields.span()));
            }
        }
    }

    Ok(TypedExpr {
        inferred_type: resolved_ty,
        kind: TypedExprKind::StructConstruction(initialized_fields).in_span(expr_span),
    })
}

#[cfg(test)]
mod tests {
    use zrc_parser::{ast::expr::Expr, lexer::NumberLiteral};
    use zrc_utils::{
        span::{Span, Spannable},
        spanned_test,
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
                    Span::from_positions_and_file(0, 9, "<test>"),
                    Expr::build_number(spanned_test!(8, NumberLiteral::Decimal("1"), 9), None)
                ),
            ),
            Ok(TypedExpr {
                inferred_type: TastType::Usize,
                kind: TypedExprKind::SizeOf(TastType::I32)
                    .in_span(Span::from_positions_and_file(0, 9, "<test>")),
            })
        );
    }
}
