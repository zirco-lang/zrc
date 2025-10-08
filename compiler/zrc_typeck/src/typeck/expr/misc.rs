//! type checking for misc expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::{expr::Expr, ty::Type};
use zrc_utils::span::{Span, Spannable, Spanned};

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

    // Check if this is an enum construction (struct with __discriminant__ and __value__ fields)
    // and the user is using variant syntax
    let is_enum_construction = if let TastType::Struct(ref struct_fields) = resolved_ty {
        struct_fields.len() == 2
            && struct_fields.contains_key("__discriminant__")
            && struct_fields.contains_key("__value__")
    } else {
        false
    };

    // If it's an enum and we have exactly one field being initialized
    if is_enum_construction && fields.value().len() == 1 {
        if let Some(field_init) = fields.value().first() {
            let (variant_name, variant_value) = field_init.value();
            let variant_name_str = variant_name.value();
            
            // Check if the provided name is a variant (not __discriminant__ or __value__)
            if !expected_fields.contains_key(variant_name_str) {
                // Try to find it in the union of variants
                if let TastType::Struct(ref struct_fields) = resolved_ty {
                    if let Some(TastType::Union(union_fields)) = struct_fields.get("__value__") {
                        // Check if the variant exists in the union
                        if let Some(variant_type) = union_fields.get(variant_name_str) {
                            // This is enum variant construction!
                            // Find the discriminant value (index in the union)
                            let discriminant_value: usize = union_fields
                                .keys()
                                .enumerate()
                                .find_map(|(idx, key)| if key == variant_name_str { Some(idx) } else { None })
                                .expect("variant should exist in union");

                            // Type check the variant value
                            let typed_variant_expr = type_expr(scope, variant_value.clone())?;

                            // Try to coerce the variant value to the expected type
                            let typed_variant_expr = if typed_variant_expr.inferred_type == *variant_type {
                                typed_variant_expr
                            } else if typed_variant_expr
                                .inferred_type
                                .can_implicitly_cast_to(variant_type)
                            {
                                try_coerce_to(typed_variant_expr, variant_type)
                            } else {
                                return Err(DiagnosticKind::ExpectedGot {
                                    expected: variant_type.to_string(),
                                    got: typed_variant_expr.inferred_type.to_string(),
                                }
                                .error_in(typed_variant_expr.kind.span()));
                            };

                            // Construct the enum as a struct with __discriminant__ and __value__
                            let mut enum_fields: IndexMap<&'input str, TypedExpr<'input>> = IndexMap::new();

                            // Add discriminant field
                            // We need to create a string that represents the discriminant value
                            use std::string::ToString;
                            let discriminant_str = discriminant_value.to_string();
                            let discriminant_str_leaked = Box::leak(discriminant_str.into_boxed_str());
                            
                            enum_fields.insert(
                                "__discriminant__",
                                TypedExpr {
                                    inferred_type: TastType::Usize,
                                    kind: TypedExprKind::NumberLiteral(
                                        zrc_parser::lexer::NumberLiteral::Decimal(discriminant_str_leaked),
                                        TastType::Usize
                                    ).in_span(variant_name.span()),
                                },
                            );

                            // Add __value__ field as a cast of the variant value to the union type
                            enum_fields.insert(
                                "__value__",
                                TypedExpr {
                                    inferred_type: TastType::Union(union_fields.clone()),
                                    kind: TypedExprKind::Cast(
                                        Box::new(typed_variant_expr),
                                        Spanned::from_span_and_value(
                                            fields.span(),
                                            TastType::Union(union_fields.clone())
                                        )
                                    ).in_span(fields.span()),
                                },
                            );

                            return Ok(TypedExpr {
                                inferred_type: resolved_ty,
                                kind: TypedExprKind::StructConstruction(enum_fields).in_span(expr_span),
                            });
                        }
                    }
                }
            }
        }
    }

    // Regular struct/union construction
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
