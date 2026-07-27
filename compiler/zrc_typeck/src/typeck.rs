//! Zirco programming language type checker

mod block;
mod declaration;
mod expr;
mod scope;
mod ty;

use std::time::Instant;

pub use block::{
	BlockMetadata, BlockReturnAbility, BlockReturnActuality, coerce_stmt_into_block,
	has_duplicates, type_block,
};
pub use declaration::process_declaration;
pub use expr::type_expr;
pub use scope::{GlobalScope, Scope, ValueEntry};
use tracing::{debug, instrument};
pub use ty::resolve_type;
use zrc_parser::ast::stmt::Declaration as AstDeclaration;
use zrc_utils::span::Spanned;

use crate::tast::stmt::TypedDeclaration;

/// # Errors
/// Errors with type checker errors.
#[instrument(skip_all)]
pub fn type_program<'input, 'gs>(
	global_scope: &'gs mut GlobalScope<'input>,
	program: Vec<Spanned<AstDeclaration<'input>>>,
) -> Result<Vec<Spanned<TypedDeclaration<'input>>>, zrc_diagnostics::Diagnostic> {
	debug!("pre-registering {} declarations", program.len());
	let pre_register_start = Instant::now();
	// Phase 1: register all declarations (mutating the global scope)
	for declaration in &program {
		declaration::register_declaration_value(global_scope, declaration.value())?;
	}

	// Phase 2: finalize all declarations (read-only access to the scope)
	let mut results: Vec<Result<Spanned<TypedDeclaration<'input>>, zrc_diagnostics::Diagnostic>> =
		Vec::with_capacity(program.len());

	debug!(
		elapsed = ?pre_register_start.elapsed(),
		"pre-registration complete, finalizing declarations"
	);
	let finalize_start = Instant::now();

	for declaration in program {
		let span = declaration.span();
		let ast_decl = declaration.into_value();

		match declaration::finalize_declaration_value(global_scope, ast_decl) {
			Ok(Some(typed_decl)) => {
				results.push(Ok(Spanned::from_span_and_value(span, typed_decl)));
				debug!(span = ?span, "declaration ok, inserting into TAST");
			}
			Ok(None) => {
				debug!(
					span = ?span,
					"declaration finalized, no runtime representation needed"
				);
			}
			Err(diag) => results.push(Err(diag)),
		}
	}
	debug!(
		elapsed = ?finalize_start.elapsed(),
		"type checking complete, program OK"
	);

	results.into_iter().collect()
}
