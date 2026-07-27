//! Utilities for Zircop lints

use std::any::type_name;

use zrc_parser::ast::stmt::Declaration as AstDecl;
use zrc_typeck::tast::stmt::TypedDeclaration;
use zrc_utils::span::Spanned;

use crate::diagnostic::LintDiagnostic;

/// Any lint in Zircop implements this trait.
pub trait Lint {
	/// Get the name of this lint
	#[must_use]
	fn name(&self) -> &'static str {
		type_name::<Self>()
	}

	/// Run a syntactic lint on a program.
	fn lint_ast(&self, _program: Vec<Spanned<AstDecl<'_>>>) -> Vec<LintDiagnostic> {
		vec![]
	}

	/// Run a semantic lint on a program.
	fn lint_tast(&self, _program: Vec<Spanned<TypedDeclaration<'_>>>) -> Vec<LintDiagnostic> {
		vec![]
	}
}
