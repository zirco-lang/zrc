//! Utilities for Zircop lints

use zrc_parser::ast::stmt::Declaration as AstDecl;
use zrc_typeck::tast::stmt::TypedDeclaration;
use zrc_utils::span::Spanned;

use crate::diagnostic::LintDiagnostic;

/// Any lint in Zircop implements this trait.
pub trait Lint {
    /// Run a syntactic lint on a program.
    fn lint_ast(&self, _program: Vec<Spanned<AstDecl<'_>>>) -> Vec<LintDiagnostic> {
        vec![]
    }

    /// Run a semantic lint on a program.
    fn lint_tast(&self, _program: Vec<Spanned<TypedDeclaration<'_, '_>>>) -> Vec<LintDiagnostic> {
        vec![]
    }
}
