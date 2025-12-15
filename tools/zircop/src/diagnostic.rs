//! Diagnostics producible by the Zircop tool

use thiserror::Error;
use zrc_diagnostics::diagnostic::GenericDiagnostic;

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
    #[error("unreachable code")]
    UnreachableCode,
    #[error("empty `if` block with an `else` block present - consider inverting the condition")]
    EmptyIfBlock,
    #[error("empty `else` block - consider removing the `else` block")]
    EmptyElseBlock,
    #[error(
        "empty `while` loop body - consider adding statements to the body or removing the loop"
    )]
    EmptyWhileBody,
}

/// A Zircop lint
pub type LintDiagnostic = GenericDiagnostic<LintDiagnosticKind>;
