//! For declarations

mod func;
mod let_decl;

pub use let_decl::process_let_declaration;
use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpannedExt};
use zrc_parser::ast::stmt::Declaration as AstDeclaration;

use super::{scope::GlobalScope, ty::resolve_type_with_self_reference, type_block};
use crate::{
    tast::{
        expr::{TypedExpr, TypedExprKind},
        stmt::TypedDeclaration,
    },
    typeck::scope::ValueEntry,
};

/// Check if an expression is a constant expression that can be evaluated at
/// compile time.
///
/// Currently, literal expressions and unary minus on literals are considered
/// constant.
#[expect(clippy::wildcard_enum_match_arm)]
pub fn is_constant_expr(expr: &TypedExpr) -> bool {
    match expr.kind.value() {
        TypedExprKind::NumberLiteral(_, _)
        | TypedExprKind::BooleanLiteral(_)
        | TypedExprKind::StringLiteral(_)
        | TypedExprKind::CharLiteral(_) => true,
        // Unary minus on a constant is also a constant
        TypedExprKind::UnaryMinus(inner) => is_constant_expr(inner),
        _ => false,
    }
}

/// Process a top-level [AST declaration](AstDeclaration), insert it into the
/// scope, and return a [TAST declaration](TypedDeclaration).
///
/// This should only be used in the global scope.
///
/// # Errors
/// Errors if a type checker error is encountered.
pub fn process_declaration<'input, 'gs>(
    global_scope: &'gs mut GlobalScope<'input>,
    declaration: AstDeclaration<'input>,
) -> Result<Option<TypedDeclaration<'input, 'gs>>, Diagnostic> {
    // Backwards-compatible wrapper: perform registration then finalize
    // for this single declaration. We operate on the AST declaration by
    // value (not a `Spanned` wrapper) because some callers pass the
    // unspanned enum directly in tests.
    register_declaration_value(global_scope, &declaration)?;
    finalize_declaration_value(global_scope, declaration)
}

/// Register top-level items into the mutable `GlobalScope` without
/// producing final typed declarations. This is the first phase of the
/// two-phase processing: registration.
/// Register from an AST declaration by reference (value-less variant).
pub fn register_declaration_value<'input>(
    global_scope: &mut GlobalScope<'input>,
    declaration: &AstDeclaration<'input>,
) -> Result<(), Diagnostic> {
    match declaration {
        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => func::register_function_declaration(
            global_scope,
            *name,
            parameters.clone(),
            return_type.clone(),
            body.clone(),
        ),

        AstDeclaration::TypeAliasDeclaration { name, ty } => {
            if global_scope.types.has(name.value()) {
                return Err(name.error(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())));
            }

            let resolved_ty = resolve_type_with_self_reference(
                &global_scope.create_subscope(),
                ty.clone(),
                name.value(),
            )?;
            global_scope.types.insert(name.value(), resolved_ty);
            Ok(())
        }

        AstDeclaration::GlobalLetDeclaration(decls) => {
            let mut scope = global_scope.create_subscope();
            let typed_declarations =
                process_let_declaration(&mut scope, decls.clone().into_value())?;

            for decl in &typed_declarations {
                if let Some(ref value) = decl.value().value
                    && !is_constant_expr(value)
                {
                    return Err(
                        DiagnosticKind::GlobalInitializerMustBeConstant.error_in(value.kind.span())
                    );
                }
            }

            for decl in &typed_declarations {
                global_scope.global_values.insert(
                    decl.value().name.value(),
                    ValueEntry::unused(decl.value().ty.clone(), decl.span()),
                );
            }

            Ok(())
        }
    }
}

/// Finalize a declaration using only immutable access to `GlobalScope`.
/// This produces the final `TypedDeclaration` where applicable.
pub fn finalize_declaration_value<'input, 'gs>(
    global_scope: &'gs GlobalScope<'input>,
    declaration: AstDeclaration<'input>,
) -> Result<Option<TypedDeclaration<'input, 'gs>>, Diagnostic> {
    match declaration {
        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => func::finalize_function_declaration(global_scope, name, parameters, return_type, body),

        AstDeclaration::TypeAliasDeclaration { .. } => Ok(None),

        AstDeclaration::GlobalLetDeclaration(decls) => {
            let mut scope = global_scope.create_subscope();
            let typed_declarations = process_let_declaration(&mut scope, decls.into_value())?;
            Ok(Some(TypedDeclaration::GlobalLetDeclaration(
                typed_declarations,
            )))
        }
    }
}
