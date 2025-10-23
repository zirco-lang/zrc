//! For declarations

mod func;
mod let_decl;

pub use let_decl::process_let_declaration;
use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpannedExt};
use zrc_parser::ast::stmt::Declaration as AstDeclaration;

use super::{scope::GlobalScope, ty::resolve_type_with_self_reference, type_block};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    stmt::TypedDeclaration,
};

/// Check if an expression is a constant expression that can be evaluated at
/// compile time.
///
/// Currently, only literal expressions are considered constant.
pub const fn is_constant_expr(expr: &TypedExpr) -> bool {
    matches!(
        expr.kind.value(),
        TypedExprKind::NumberLiteral(_, _)
            | TypedExprKind::BooleanLiteral(_)
            | TypedExprKind::StringLiteral(_)
            | TypedExprKind::CharLiteral(_)
    )
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
            name,
            parameters,
            return_type,
            body,
        } => func::process_function_declaration(global_scope, name, parameters, return_type, body)?,

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

            // Validate that all initializers are constant expressions
            for decl in &typed_declarations {
                if let Some(ref value) = decl.value().value
                    && !is_constant_expr(value)
                {
                    return Err(
                        DiagnosticKind::GlobalInitializerMustBeConstant.error_in(value.kind.span())
                    );
                }
            }

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
