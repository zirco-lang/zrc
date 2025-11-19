//! Lint pass tooling for Zircop

use zrc_parser::ast::stmt::Declaration;
use zrc_typeck::tast::stmt::TypedDeclaration;
use zrc_utils::span::Spanned;

use crate::{diagnostic::LintDiagnostic, lint::Lint};

/// A passlist is a list of lints that can be evaluated on some source code.
#[expect(missing_debug_implementations)]
pub struct PassList(pub Vec<Box<dyn Lint>>);

impl PassList {
    /// Create a new passlist from a list of lints
    #[must_use]
    pub fn new(lints: Vec<Box<dyn Lint>>) -> Self {
        Self(lints)
    }

    /// Invoke all lints in this passlist on the given AST. This clones the
    /// program for each lint to allow multiple lints to consume it.
    #[must_use]
    pub fn lint_ast(&self, program: &[Spanned<Declaration>]) -> Vec<LintDiagnostic> {
        let mut diagnostics = vec![];
        for lint in &self.0 {
            // Clone to allow multiple lints to consume the program
            diagnostics.extend(lint.lint_ast(program.to_vec()));
        }
        diagnostics
    }

    /// Invoke all lints in this passlist on the given TAST. This clones the
    /// program for each lint to allow multiple lints to consume it.
    #[must_use]
    pub fn lint_tast(&self, program: &[Spanned<TypedDeclaration<'_, '_>>]) -> Vec<LintDiagnostic> {
        let mut diagnostics = vec![];
        for lint in &self.0 {
            // Clone to allow multiple lints to consume the program
            diagnostics.extend(lint.lint_tast(program.to_vec()));
        }
        diagnostics
    }
}
