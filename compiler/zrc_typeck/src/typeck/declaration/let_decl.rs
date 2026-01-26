//! Process let declarations during type checking

use zrc_diagnostics::{Diagnostic, DiagnosticKind, LabelKind, diagnostic::GenericLabel};
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
#[expect(clippy::too_many_lines)]
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

                let ty_span = let_declaration.ty.as_ref().map(|x| x.0.span());
                let resolved_ty = let_declaration
                    .ty
                    .map(|ty| resolve_type(scope, ty))
                    .transpose()?;

                let result_decl = match (typed_expr, resolved_ty) {
                    (None, None) => {
                        return Err(DiagnosticKind::NoTypeNoValue.error_in(let_decl_span));
                    }

                    // Explicitly typed with no value
                    (None, Some(ty)) => {
                        // Check if trying to declare a variable with function type
                        if matches!(ty, TastType::Fn(_)) {
                            return Err(
                                DiagnosticKind::FunctionNotFirstClass.error_in(let_decl_span)
                            );
                        }
                        TastLetDeclaration {
                            name: let_declaration.name,
                            ty,
                            value: None,
                            is_constant: let_declaration.is_constant,
                        }
                    }

                    // Infer type from value
                    (
                        Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                        None,
                    ) => {
                        // Check if trying to infer a function type
                        if matches!(inferred_type, TastType::Fn(_)) {
                            return Err(
                                DiagnosticKind::FunctionNotFirstClass.error_in(let_decl_span)
                            );
                        }

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
                            is_constant: let_declaration.is_constant,
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
                        // Check if trying to use a function type
                        if matches!(resolved_ty, TastType::Fn(_)) {
                            return Err(
                                DiagnosticKind::FunctionNotFirstClass.error_in(let_decl_span)
                            );
                        }

                        if inferred_type == resolved_ty {
                            TastLetDeclaration {
                                name: let_declaration.name,
                                ty: inferred_type.clone(),
                                value: Some(TypedExpr {
                                    inferred_type,
                                    kind,
                                }),
                                is_constant: let_declaration.is_constant,
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
                                is_constant: let_declaration.is_constant,
                            }
                        } else {
                            return Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                                expected: resolved_ty.to_string(),
                                got: inferred_type.to_string(),
                            }
                            .error_in(let_decl_span)
                            .with_label(GenericLabel::note(
                                LabelKind::VariableDeclaredType(resolved_ty.to_string())
                                    .in_span(ty_span.expect("ty_span should exist here")),
                            ))
                            .with_label(GenericLabel::error(
                                LabelKind::InvalidAssignmentRightHandSideType {
                                    expected: resolved_ty.to_string(),
                                    got: inferred_type.to_string(),
                                }
                                .in_span(kind.span()),
                            )));
                        }
                    }
                };

                scope.values.insert(
                    result_decl.name.value(),
                    ValueEntry {
                        ty: result_decl.ty.clone(),
                        declaration_span: let_decl_span,
                        is_constant: let_declaration.is_constant,
                        referenced_spans: vec![],
                    },
                );
                Ok(result_decl.in_span(let_decl_span))
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()
}

#[cfg(test)]
mod tests {
    use zrc_diagnostics::{DiagnosticKind, Severity};
    use zrc_parser::parser::parse_program;

    use crate::typeck::{scope::GlobalScope, type_program};

    #[test]
    fn test_function_type_in_let_declaration_is_rejected() {
        let code = "fn some_function() {}\n\n\
                    fn main() -> i32 {\n\
                    \x20   let a = some_function;\n\
                    \x20   return 0;\n\
                    }\n";

        let mut global_scope = GlobalScope::new();
        let ast = parse_program(code, "<test>").expect("parsing should succeed");
        let result = type_program(&mut global_scope, ast);

        assert!(result.is_err());
        if let Err(diagnostic) = result {
            assert_eq!(diagnostic.severity, Severity::Error);
            assert!(matches!(
                diagnostic.kind.into_value(),
                DiagnosticKind::FunctionNotFirstClass
            ));
        }
    }

    #[test]
    fn test_explicit_function_type_in_let_declaration_is_rejected() {
        let code = "fn some_function() -> i32 { return 0; }\n\n\
                    fn main() -> i32 {\n\
                    \x20   let a: fn() -> i32;\n\
                    \x20   return 0;\n\
                    }\n";

        let mut global_scope = GlobalScope::new();
        let ast = parse_program(code, "<test>").expect("parsing should succeed");
        let result = type_program(&mut global_scope, ast);

        assert!(result.is_err());
        if let Err(diagnostic) = result {
            assert_eq!(diagnostic.severity, Severity::Error);
            assert!(matches!(
                diagnostic.kind.into_value(),
                DiagnosticKind::FunctionNotFirstClass
            ));
        }
    }
}
