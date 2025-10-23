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
use crate::tast::{
    self,
    stmt::{ArgumentDeclaration as TastArgumentDeclaration, TypedDeclaration},
    ty::{Fn, FunctionDeclarationGlobalMetadata, Type as TastType},
};

/// Process a top-level function declaration, insert it into the
/// scope, and return a [TAST declaration](TypedDeclaration).
///
/// This should only be used in the global scope.
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines, clippy::needless_pass_by_value)]
pub fn process_function_declaration<'input>(
    global_scope: &mut GlobalScope<'input>,
    name: Spanned<&'input str>,
    parameters: Spanned<ArgumentDeclarationList<'input>>,
    return_type: Option<Type<'input>>,
    body: Option<Spanned<Vec<Stmt<'input>>>>,
) -> Result<Option<TypedDeclaration<'input>>, Diagnostic> {
    if matches!(parameters.value(), ArgumentDeclarationList::Variadic(_)) && body.is_some() {
        return Err(parameters.error(|_| DiagnosticKind::VariadicFunctionMustBeExternal));
    }

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
                tast::stmt::ArgumentDeclarationList::NonVariadic(resolved_parameters.clone())
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

                let canonical = global_scope
                    .declarations
                    .get(name.value())
                    .expect("global_scope.declarations was not populated with function properly");

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
                return Err(name.error(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())));
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

    if *name.value() == "main" {
        // main() can be either
        // fn() -> i32
        // fn(usize, **u8) -> i32

        if resolved_return_type != TastType::I32 {
            return Err(name.error(|_| {
                DiagnosticKind::MainFunctionMustReturnI32(resolved_return_type.to_string())
            }));
        }

        match &parameters.value() {
            ArgumentDeclarationList::NonVariadic(params) if params.is_empty() => {
                // can be either empty or (usize, **u8)
            }

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
}
