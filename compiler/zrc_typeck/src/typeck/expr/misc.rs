//! type checking for misc expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::{expr::Expr, ty::Type};
use zrc_utils::span::{Span, Spannable};

use super::{
    super::scope::Scope,
    helpers::{expect, try_coerce_to},
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

    // Handle {int} type resolution in ternary branches
    let (result_type, if_true_final, if_false_final) =
        if if_true_t.inferred_type == if_false_t.inferred_type {
            // Both branches have the same type
            if matches!(if_true_t.inferred_type, TastType::Int) {
                // Both are {int}, resolve to i32
                let if_true_resolved = try_coerce_to(if_true_t, &TastType::I32);
                let if_false_resolved = try_coerce_to(if_false_t, &TastType::I32);
                (TastType::I32, if_true_resolved, if_false_resolved)
            } else {
                (if_true_t.inferred_type.clone(), if_true_t, if_false_t)
            }
        } else if if_true_t
            .inferred_type
            .can_implicitly_cast_to(&if_false_t.inferred_type)
        {
            // if_true can coerce to if_false type
            let if_true_coerced = try_coerce_to(if_true_t, &if_false_t.inferred_type);
            (
                if_false_t.inferred_type.clone(),
                if_true_coerced,
                if_false_t,
            )
        } else if if_false_t
            .inferred_type
            .can_implicitly_cast_to(&if_true_t.inferred_type)
        {
            // if_false can coerce to if_true type
            let if_false_coerced = try_coerce_to(if_false_t, &if_true_t.inferred_type);
            (if_true_t.inferred_type.clone(), if_true_t, if_false_coerced)
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
        | TastType::Int
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

        // Try to coerce the field value to the expected type
        let typed_field_expr = if typed_field_expr.inferred_type == *expected_type {
            typed_field_expr
        } else if typed_field_expr
            .inferred_type
            .can_implicitly_cast_to(expected_type)
        {
            try_coerce_to(typed_field_expr, expected_type)
        } else {
            return Err(DiagnosticKind::ExpectedGot {
                expected: expected_type.to_string(),
                got: typed_field_expr.inferred_type.to_string(),
            }
            .error_in(typed_field_expr.kind.span()));
        };

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

/// Typeck an array literal
///
/// Array literals like `[1, 2, 3] :: *i32` are typed as follows:
/// 1. All elements must have compatible types
/// 2. The type annotation (if present) specifies the result pointer type
/// 3. Without type annotation, we infer from the first element
pub fn type_expr_array_literal<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    elements: Vec<Expr<'input>>,
    ty_annotation: Option<Type<'input>>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    // Determine the target element type
    let (result_type, target_element_type) = if let Some(ty_ann) = ty_annotation {
        // Type annotation provided - resolve it
        let resolved_type = resolve_type(scope.types, ty_ann)?;

        // The type annotation must be a pointer type
        let element_type = match &resolved_type {
            TastType::Ptr(elem_ty) => (**elem_ty).clone(),
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
            | TastType::Int
            | TastType::Fn(_)
            | TastType::Struct(_)
            | TastType::Union(_)
            | TastType::Opaque(_) => {
                return Err(DiagnosticKind::ExpectedGot {
                    expected: "pointer type".to_string(),
                    got: resolved_type.to_string(),
                }
                .error_in(expr_span));
            }
        };

        (resolved_type, Some(element_type))
    } else {
        // No type annotation - will infer from first element
        (TastType::Int, None) // placeholder, will be updated
    };

    // Type check all elements and coerce to target type
    let mut typed_elements = Vec::new();
    let mut inferred_element_type: Option<TastType<'input>> = None;

    for element in elements {
        let element_span = element.0.span();
        let mut typed_element = type_expr(scope, element)?;

        if let Some(ref target_type) = target_element_type {
            // Type annotation provided - coerce elements to target type
            if typed_element.inferred_type != *target_type {
                if typed_element
                    .inferred_type
                    .can_implicitly_cast_to(target_type)
                {
                    typed_element = try_coerce_to(typed_element, target_type);
                } else {
                    return Err(DiagnosticKind::ExpectedGot {
                        expected: target_type.to_string(),
                        got: typed_element.inferred_type.to_string(),
                    }
                    .error_in(element_span));
                }
            }
        } else {
            // No type annotation - infer from elements
            if let Some(ref expected_type) = inferred_element_type {
                // All elements must have compatible types
                if typed_element.inferred_type != *expected_type {
                    if typed_element
                        .inferred_type
                        .can_implicitly_cast_to(expected_type)
                    {
                        typed_element = try_coerce_to(typed_element, expected_type);
                    } else if expected_type.can_implicitly_cast_to(&typed_element.inferred_type) {
                        // Update inferred type to wider type
                        inferred_element_type = Some(typed_element.inferred_type.clone());
                    } else {
                        return Err(DiagnosticKind::ExpectedSameType(
                            expected_type.to_string(),
                            typed_element.inferred_type.to_string(),
                        )
                        .error_in(element_span));
                    }
                }
            } else {
                // First element sets the inferred type
                inferred_element_type = Some(typed_element.inferred_type.clone());
            }
        }

        typed_elements.push(typed_element);
    }

    // Determine final result type
    let final_result_type = if target_element_type.is_some() {
        result_type
    } else {
        // No type annotation - infer pointer to element type
        if let Some(elem_ty) = inferred_element_type {
            // Resolve {int} type to i32
            let resolved_elem_ty = if matches!(elem_ty, TastType::Int) {
                TastType::I32
            } else {
                elem_ty
            };

            // Coerce all elements to the resolved type if needed
            if matches!(resolved_elem_ty, TastType::I32) {
                typed_elements = typed_elements
                    .into_iter()
                    .map(|element| {
                        if element.inferred_type == TastType::Int {
                            try_coerce_to(element, &TastType::I32)
                        } else {
                            element
                        }
                    })
                    .collect();
            }

            TastType::Ptr(Box::new(resolved_elem_ty))
        } else {
            // Empty array without type annotation - error
            return Err(DiagnosticKind::NoTypeNoValue.error_in(expr_span));
        }
    };

    Ok(TypedExpr {
        inferred_type: final_result_type,
        kind: TypedExprKind::ArrayLiteral(typed_elements).in_span(expr_span),
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
                kind: TypedExprKind::SizeOf(TastType::Int)
                    .in_span(Span::from_positions_and_file(0, 9, "<test>")),
            })
        );
    }
}
