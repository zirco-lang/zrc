//! Process function declarations

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpannedExt};
use zrc_parser::ast::{
    stmt::{ArgumentDeclarationList, Stmt},
    ty::Type,
};
use zrc_utils::span::{Spannable, Spanned};

use super::{
    super::{block::BlockReturnAbility, resolve_type, scope::GlobalScope},
    type_block,
};
use crate::{
    tast::{
        self,
        stmt::{ArgumentDeclaration as TastArgumentDeclaration, TypedDeclaration},
        ty::{Fn, FunctionDeclarationGlobalMetadata, Type as TastType},
    },
    typeck::scope::ValueEntry,
};

/// Register the function signature and global entries into `global_scope`.
/// This does not typecheck the function body; it only inserts the function
/// into the global value and declaration tables so other declarations can
/// resolve it during registration.
#[expect(clippy::needless_pass_by_value)]
pub fn register_function_declaration<'input>(
    global_scope: &mut GlobalScope<'input>,
    name: Spanned<&'input str>,
    parameters: Spanned<ArgumentDeclarationList<'input>>,
    return_type: Option<Type<'input>>,
    body: Option<Spanned<Vec<Stmt<'input>>>>,
) -> Result<(), Diagnostic> {
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
                tast::stmt::ArgumentDeclarationList::NonVariadic(resolved_parameters.clone())
            }
            ArgumentDeclarationList::Variadic(_) => {
                tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
            }
        },
        returns: Box::new(resolved_return_type.clone()),
    };

    let has_existing_implementation =
        if let Some(ty_rc) = global_scope.global_values.resolve(name.value()) {
            let ty = ty_rc.borrow();
            if let TastType::Fn(_) = ty.ty {
                let canonical = global_scope
                    .declarations
                    .get(name.value())
                    .expect("global_scope.declarations was not populated with function properly");

                if !canonical.fn_type.types_equal(&fn_type) {
                    return Err(name.error(|_| {
                        DiagnosticKind::ConflictingFunctionDeclarations(
                            canonical.fn_type.to_string(),
                            fn_type.to_string(),
                        )
                    }));
                }

                if body.is_some() && canonical.has_implementation {
                    return Err(name.error(|name| {
                        DiagnosticKind::ConflictingImplementations(name.to_string())
                    }));
                }

                canonical.has_implementation
            } else {
                return Err(name.error(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())));
            }
        } else {
            false
        };

    global_scope.global_values.insert(
        name.into_value(),
        ValueEntry::unused(TastType::Fn(fn_type.clone()), name.span()),
    );

    global_scope.declarations.insert(
        name.into_value(),
        FunctionDeclarationGlobalMetadata {
            fn_type,
            has_implementation: body.is_some() || has_existing_implementation,
        },
    );

    if *name.value() == "main" {
        if resolved_return_type != TastType::I32 {
            return Err(name.error(|_| {
                DiagnosticKind::MainFunctionMustReturnI32(resolved_return_type.to_string())
            }));
        }

        match &parameters.value() {
            ArgumentDeclarationList::NonVariadic(params) if params.is_empty() => {}
            ArgumentDeclarationList::NonVariadic(params) if params.len() == 2 => {
                let first_param_type =
                    resolve_type(&global_scope.types, params[0].value().ty.clone())?;
                let second_param_type =
                    resolve_type(&global_scope.types, params[1].value().ty.clone())?;

                if first_param_type != TastType::Usize
                    || second_param_type
                        .into_pointee()
                        .map(tast::ty::Type::into_pointee)
                        != Some(Some(TastType::U8))
                {
                    return Err(name.error(|_| DiagnosticKind::MainFunctionInvalidParameters));
                }
            }
            ArgumentDeclarationList::NonVariadic(_) => {
                return Err(name.error(|_| DiagnosticKind::MainFunctionInvalidParameters));
            }
            ArgumentDeclarationList::Variadic(_) => {
                return Err(name.error(|_| DiagnosticKind::MainFunctionInvalidParameters));
            }
        }
    }

    Ok(())
}

/// Finalize the function declaration using only immutable access to the
/// `GlobalScope`. This constructs the `TypedDeclaration` and typechecks the
/// body (if any) using a subscope derived from `global_scope`.
#[expect(clippy::needless_pass_by_value)]
pub fn finalize_function_declaration<'input, 'gs>(
    global_scope: &'gs GlobalScope<'input>,
    name: Spanned<&'input str>,
    parameters: Spanned<ArgumentDeclarationList<'input>>,
    return_type: Option<Type<'input>>,
    body: Option<Spanned<Vec<Stmt<'input>>>>,
) -> Result<Option<TypedDeclaration<'input, 'gs>>, Diagnostic> {
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

    Ok(Some(TypedDeclaration::FunctionDeclaration {
        name,
        parameters: match parameters.value() {
            ArgumentDeclarationList::NonVariadic(_) => {
                tast::stmt::ArgumentDeclarationList::NonVariadic(resolved_parameters.clone())
            }
            ArgumentDeclarationList::Variadic(_) => {
                tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
            }
        }
        .in_span(parameters.span()),
        return_type: resolved_return_type.clone().in_span(
            return_type
                .as_ref()
                .map_or_else(|| name.span(), |ty| ty.0.span()),
        ),
        body: if let Some(body) = body {
            let mut function_scope = global_scope.create_subscope();
            for param in resolved_parameters {
                function_scope.values.insert(
                    param.name.value(),
                    ValueEntry::unused(param.ty.into_value(), param.name.span()),
                );
            }

            Some(body.span().containing(type_block(
                &function_scope,
                body,
                false,
                BlockReturnAbility::MustReturn(resolved_return_type),
            )?))
        } else {
            None
        },
    }))
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
    use zrc_utils::spanned_test;

    use super::*;
    use crate::typeck::scope::{TypeCtx, ValueCtx};

    #[test]
    fn re_declaration_works_as_expected() {
        assert!(
            super::super::process_declaration(
                &mut GlobalScope {
                    global_values: ValueCtx::from_unused([(
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
                    name: spanned_test!(0, "get_true", 0),
                    parameters: spanned_test!(
                        0,
                        AstArgumentDeclarationList::NonVariadic(vec![]),
                        0
                    ),
                    return_type: Some(Type(spanned_test!(0, TypeKind::Identifier("bool"), 0))),
                    body: Some(spanned_test!(
                        0,
                        vec![Stmt(spanned_test!(
                            0,
                            StmtKind::ReturnStmt(Some(Expr(spanned_test!(
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

    #[test]
    fn re_declaration_with_different_spans_is_accepted() {
        use crate::tast::stmt::ArgumentDeclaration as TastArgumentDeclaration;

        // First declaration at span 0..10
        let mut scope = GlobalScope {
            global_values: ValueCtx::from_unused([(
                "read",
                TastType::Fn(Fn {
                    arguments: TastArgumentDeclarationList::NonVariadic(vec![
                        TastArgumentDeclaration {
                            name: spanned_test!(5, "buffer", 11),
                            ty: spanned_test!(13, TastType::Ptr(Box::new(TastType::U8)), 16),
                        },
                        TastArgumentDeclaration {
                            name: spanned_test!(18, "start", 23),
                            ty: spanned_test!(25, TastType::Usize, 30),
                        },
                    ]),
                    returns: Box::new(TastType::Usize),
                }),
            )]),
            types: TypeCtx::from([("u8", TastType::U8), ("usize", TastType::Usize)]),
            declarations: HashMap::from([(
                "read",
                FunctionDeclarationGlobalMetadata {
                    fn_type: Fn {
                        arguments: TastArgumentDeclarationList::NonVariadic(vec![
                            TastArgumentDeclaration {
                                name: spanned_test!(5, "buffer", 11),
                                ty: spanned_test!(13, TastType::Ptr(Box::new(TastType::U8)), 16),
                            },
                            TastArgumentDeclaration {
                                name: spanned_test!(18, "start", 23),
                                ty: spanned_test!(25, TastType::Usize, 30),
                            },
                        ]),
                        returns: Box::new(TastType::Usize),
                    },
                    has_implementation: false,
                },
            )]),
        };

        // Second declaration at span 50..60 (different spans but same types)
        let result = super::super::process_declaration(
            &mut scope,
            AstDeclaration::FunctionDeclaration {
                name: spanned_test!(53, "read", 57),
                parameters: spanned_test!(
                    58,
                    AstArgumentDeclarationList::NonVariadic(vec![
                        spanned_test!(
                            59,
                            zrc_parser::ast::stmt::ArgumentDeclaration {
                                name: spanned_test!(60, "buffer", 66),
                                ty: Type(spanned_test!(
                                    68,
                                    TypeKind::Ptr(Box::new(Type(spanned_test!(
                                        69,
                                        TypeKind::Identifier("u8"),
                                        71
                                    )))),
                                    72
                                )),
                            },
                            73
                        ),
                        spanned_test!(
                            74,
                            zrc_parser::ast::stmt::ArgumentDeclaration {
                                name: spanned_test!(75, "start", 80),
                                ty: Type(spanned_test!(82, TypeKind::Identifier("usize"), 87)),
                            },
                            88
                        ),
                    ]),
                    89
                ),
                return_type: Some(Type(spanned_test!(91, TypeKind::Identifier("usize"), 96))),
                body: None,
            },
        );

        // Should succeed because the types are the same, even though the spans differ
        assert!(result.is_ok());
    }
}
