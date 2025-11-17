//! Process let declarations during type checking

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_parser::ast::stmt::LetDeclaration as AstLetDeclaration;
use zrc_utils::span::{Spannable, Spanned};

use super::super::{expr::try_coerce_to, resolve_type, scope::Scope, type_expr};
use crate::{
    tast::{expr::TypedExpr, stmt::LetDeclaration as TastLetDeclaration, ty::Type as TastType},
    typeck::scope::ValueEntry,
};

/// Process a vector of [AST let declarations](AstLetDeclaration) and insert it
/// into the scope, returning a vector of [TAST let
/// declarations](TastLetDeclaration).
///
/// # Errors
/// Errors with type checker errors.
pub fn process_let_declaration<'input>(
    scope: &mut Scope<'input, '_>,
    declarations: Vec<Spanned<AstLetDeclaration<'input>>>,
) -> Result<Vec<Spanned<TastLetDeclaration<'input>>>, Diagnostic> {
    declarations
        .into_iter()
        .map(
            |let_declaration| -> Result<Spanned<TastLetDeclaration>, Diagnostic> {
                let let_decl_span = let_declaration.span();
                let let_declaration = let_declaration.into_value();

                let typed_expr = let_declaration
                    .value
                    .map(|expr| type_expr(scope, expr))
                    .transpose()?;

                let resolved_ty = let_declaration
                    .ty
                    .map(|ty| resolve_type(scope.types, ty))
                    .transpose()?;

                let result_decl = match (typed_expr, resolved_ty) {
                    (None, None) => {
                        return Err(DiagnosticKind::NoTypeNoValue.error_in(let_decl_span));
                    }

                    // Explicitly typed with no value
                    (None, Some(ty)) => TastLetDeclaration {
                        name: let_declaration.name,
                        ty,
                        value: None,
                    },

                    // Infer type from value
                    (
                        Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                        None,
                    ) => {
                        // If the inferred type is {int}, resolve it to i32
                        let resolved_type = if matches!(inferred_type, TastType::Int) {
                            TastType::I32
                        } else {
                            inferred_type.clone()
                        };

                        let value_coerced = try_coerce_to(
                            TypedExpr {
                                inferred_type,
                                kind,
                            },
                            &resolved_type,
                        );

                        TastLetDeclaration {
                            name: let_declaration.name,
                            ty: resolved_type,
                            value: Some(value_coerced),
                        }
                    }

                    // Both explicitly typed and inferable
                    (
                        Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                        Some(resolved_ty),
                    ) => {
                        if inferred_type == resolved_ty {
                            TastLetDeclaration {
                                name: let_declaration.name,
                                ty: inferred_type.clone(),
                                value: Some(TypedExpr {
                                    inferred_type,
                                    kind,
                                }),
                            }
                        } else if inferred_type.can_implicitly_cast_to(&resolved_ty) {
                            // Insert implicit cast (e.g., {int} -> i8)
                            let value_coerced = try_coerce_to(
                                TypedExpr {
                                    inferred_type,
                                    kind,
                                },
                                &resolved_ty,
                            );
                            TastLetDeclaration {
                                name: let_declaration.name,
                                ty: resolved_ty,
                                value: Some(value_coerced),
                            }
                        } else {
                            return Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                                expected: resolved_ty.to_string(),
                                got: inferred_type.to_string(),
                            }
                            .error_in(let_decl_span));
                        }
                    }
                };

                scope.values.insert(
                    result_decl.name.value(),
                    ValueEntry::unused(result_decl.ty.clone(), let_decl_span),
                );
                Ok(result_decl.in_span(let_decl_span))
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()
}
