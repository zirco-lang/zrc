//! For declarations

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpannedExt};
use zrc_parser::ast::stmt::{
    ArgumentDeclarationList, Declaration as AstDeclaration, LetDeclaration as AstLetDeclaration,
};
use zrc_utils::span::{Spannable, Spanned};

use super::{
    block::BlockReturnAbility,
    resolve_type,
    scope::{GlobalScope, Scope},
    ty::resolve_type_with_self_reference,
    type_block, type_expr,
};
use crate::tast::{
    self,
    expr::TypedExpr,
    stmt::{
        ArgumentDeclaration as TastArgumentDeclaration, LetDeclaration as TastLetDeclaration,
        TypedDeclaration,
    },
    ty::{Fn, FunctionDeclarationGlobalMetadata, Type as TastType},
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
                    ) => TastLetDeclaration {
                        name: let_declaration.name,
                        ty: inferred_type.clone(),
                        value: Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                    },

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
                        } else {
                            return Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                                expected: resolved_ty.to_string(),
                                got: inferred_type.to_string(),
                            }
                            .error_in(let_decl_span));
                        }
                    }
                };

                scope
                    .values
                    .insert(result_decl.name.value(), result_decl.ty.clone());
                Ok(result_decl.in_span(let_decl_span))
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()
}

/// Process a top-level [AST declaration](AstDeclaration), insert it into the
/// scope, and return a [TAST declaration](TypedDeclaration).
///
/// This should only be used in the global scope.
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines, clippy::missing_panics_doc)]
pub fn process_declaration<'input>(
    global_scope: &mut GlobalScope<'input>,
    declaration: AstDeclaration<'input>,
) -> Result<Option<TypedDeclaration<'input>>, Diagnostic> {
    Ok(match declaration {
        AstDeclaration::FunctionDeclaration {
            parameters,
            body: Some(_),
            ..
        } if matches!(parameters.value(), ArgumentDeclarationList::Variadic(_)) => {
            return Err(parameters.error(|_| DiagnosticKind::VariadicFunctionMustBeExternal));
        }

        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => {
            let return_type_span = return_type
                .as_ref()
                .map_or_else(|| name.span(), |ty| ty.0.span());

            let resolved_return_type = return_type
                .clone()
                .map(|ty| resolve_type(&global_scope.types, ty))
                .transpose()?
                .unwrap_or_else(TastType::unit);

            let (ArgumentDeclarationList::NonVariadic(inner_params)
            | ArgumentDeclarationList::Variadic(inner_params)) = parameters.value();

            let resolved_parameters = inner_params
                .iter()
                .map(|parameter| -> Result<TastArgumentDeclaration, Diagnostic> {
                    Ok(TastArgumentDeclaration {
                        name: parameter.value().name,
                        ty: resolve_type(&global_scope.types, parameter.value().ty.clone())?
                            .in_span(parameter.span()),
                    })
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?;

            let fn_type = Fn {
                arguments: match parameters.value() {
                    ArgumentDeclarationList::NonVariadic(_) => {
                        tast::stmt::ArgumentDeclarationList::NonVariadic(
                            resolved_parameters.clone(),
                        )
                    }
                    ArgumentDeclarationList::Variadic(_) => {
                        tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
                    }
                },
                returns: Box::new(resolved_return_type.clone()),
            };

            let has_existing_implementation =
                if let Some(ty) = global_scope.global_values.resolve(name.value()) {
                    if let TastType::Fn(_) = ty {
                        // if a function has already been declared with this name...

                        let canonical = global_scope.declarations.get(name.value()).expect(
                            "global_scope.declarations was not populated with function properly",
                        );

                        // TODO: store and reference previous declaration's span in the error
                        if canonical.fn_type != fn_type {
                            return Err(name.error(|_| {
                                DiagnosticKind::ConflictingFunctionDeclarations(
                                    canonical.fn_type.to_string(),
                                    fn_type.to_string(),
                                )
                            }));
                        }

                        // TODO: store and reference previous declaration's span in the error
                        if body.is_some() && canonical.has_implementation {
                            return Err(name.error(|name| {
                                DiagnosticKind::ConflictingImplementations(name.to_string())
                            }));
                        }

                        canonical.has_implementation
                    } else {
                        return Err(
                            name.error(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string()))
                        );
                    }
                } else {
                    false
                };

            global_scope
                .global_values
                .insert(name.into_value(), TastType::Fn(fn_type.clone()));

            global_scope.declarations.insert(
                name.into_value(),
                FunctionDeclarationGlobalMetadata {
                    fn_type,
                    has_implementation: body.is_some() || has_existing_implementation,
                },
            );

            Some(TypedDeclaration::FunctionDeclaration {
                name,
                parameters: match parameters.value() {
                    ArgumentDeclarationList::NonVariadic(_) => {
                        tast::stmt::ArgumentDeclarationList::NonVariadic(
                            resolved_parameters.clone(),
                        )
                    }
                    ArgumentDeclarationList::Variadic(_) => {
                        tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
                    }
                }
                .in_span(parameters.span()),
                return_type: resolved_return_type.clone().in_span(return_type_span),
                body: if let Some(body) = body {
                    let mut function_scope = global_scope.create_subscope();
                    for param in resolved_parameters {
                        function_scope
                            .values
                            .insert(param.name.value(), param.ty.into_value());
                    }

                    // discard return actuality as it's guaranteed
                    Some(
                        body.span().containing(
                            type_block(
                                &function_scope,
                                body,
                                false,
                                BlockReturnAbility::MustReturn(resolved_return_type),
                            )?
                            .0,
                        ),
                    )
                } else {
                    None
                },
            })
        }
        AstDeclaration::TypeAliasDeclaration { name, ty } => {
            if global_scope.types.has(name.value()) {
                return Err(name.error(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())));
            }

            let resolved_ty =
                resolve_type_with_self_reference(&global_scope.types, ty, name.value())?;

            global_scope.types.insert(name.value(), resolved_ty.clone());

            None
        }
        AstDeclaration::GlobalLetDeclaration(declarations) => {
            let mut scope = global_scope.create_subscope();
            let typed_declarations =
                process_let_declaration(&mut scope, declarations.into_value())?;

            // Add global variables to the global scope
            for decl in &typed_declarations {
                global_scope
                    .global_values
                    .insert(decl.value().name.value(), decl.value().ty.clone());
            }

            Some(TypedDeclaration::GlobalLetDeclaration(typed_declarations))
        }
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use tast::stmt::ArgumentDeclarationList as TastArgumentDeclarationList;
    use zrc_parser::ast::{
        expr::{Expr, ExprKind},
        stmt::{
            ArgumentDeclarationList as AstArgumentDeclarationList, Declaration as AstDeclaration,
            Stmt, StmtKind,
        },
        ty::{Type, TypeKind},
    };
    use zrc_utils::spanned;

    use super::*;
    use crate::typeck::scope::{TypeCtx, ValueCtx};

    #[test]
    fn re_declaration_works_as_expected() {
        assert!(
            process_declaration(
                &mut GlobalScope {
                    global_values: ValueCtx::from([(
                        "get_true",
                        TastType::Fn(Fn {
                            arguments: TastArgumentDeclarationList::NonVariadic(vec![]),
                            returns: Box::new(TastType::Bool)
                        })
                    )]),
                    types: TypeCtx::from([("bool", TastType::Bool)]),
                    declarations: HashMap::from([(
                        "get_true",
                        FunctionDeclarationGlobalMetadata {
                            fn_type: Fn {
                                arguments: TastArgumentDeclarationList::NonVariadic(vec![]),
                                returns: Box::new(TastType::Bool)
                            },
                            has_implementation: false
                        }
                    )])
                },
                AstDeclaration::FunctionDeclaration {
                    name: spanned!(0, "get_true", 0),
                    parameters: spanned!(0, AstArgumentDeclarationList::NonVariadic(vec![]), 0),
                    return_type: Some(Type(spanned!(0, TypeKind::Identifier("bool"), 0))),
                    body: Some(spanned!(
                        0,
                        vec![Stmt(spanned!(
                            0,
                            StmtKind::ReturnStmt(Some(Expr(spanned!(
                                0,
                                ExprKind::BooleanLiteral(true),
                                0
                            )))),
                            0
                        ))],
                        0
                    ))
                }
            )
            .is_ok()
        );
    }
}
