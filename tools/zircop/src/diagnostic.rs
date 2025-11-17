//! Diagnostics producible by the Zircop tool

use std::error::Error;

use derive_more::Display;
use thiserror::Error;
use zrc_diagnostics::diagnostic::{GenericDiagnostic, Severity};
use zrc_utils::span::Spanned;

/// The list of possible lints Zircop can raise
#[expect(missing_docs)]
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum LintDiagnosticKind {
    #[error("assignment found in condition expression, consider using `==` or `!=` instead")]
    AssignmentInCondition,
    #[error("empty struct used, consider using `void` instead")]
    EmptyStructUsed,
    #[error("underscore variable `{0}` used, consider renaming it")]
    UnderscoreVariableUsed(String),
    #[error("unused variable `{0}`")]
    UnusedVariable(String),
}

/// A Zircop lint
#[derive(Display, Debug, PartialEq, Eq)]
#[display("{_0}")]
pub struct LintDiagnostic(GenericDiagnostic<LintDiagnosticKind>);

impl LintDiagnostic {
    /// Create a new lint diagnostic with warning severity
    #[must_use]
    pub const fn new(kind: Spanned<LintDiagnosticKind>) -> Self {
        Self(GenericDiagnostic(Severity::Warning, kind))
    }

    /// Convert this [`LintDiagnostic`] to a printable string using ariadne. The
    /// source code is provided directly as a string if it is not read from a
    /// file.
    ///
    /// # Panics
    /// This function may panic if the span is invalid or if writing to the
    /// buffer fails.
    #[must_use]
    pub fn print(&self, piped_source: Option<&str>) -> String {
        self.0.print(piped_source)
    }
}
impl Error for LintDiagnostic {}
