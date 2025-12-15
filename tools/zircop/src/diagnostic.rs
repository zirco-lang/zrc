//! Diagnostics producible by the Zircop tool

use thiserror::Error;
use zrc_diagnostics::diagnostic::{ErrorCode, GenericDiagnostic};

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
impl ErrorCode for LintDiagnosticKind {
    fn error_code(&self) -> &'static str {
        match self {
            Self::AssignmentInCondition => "assignment_in_condition",
            Self::EmptyStructUsed => "empty_struct_used",
            Self::UnderscoreVariableUsed(_) => "underscore_variable_used",
            Self::UnusedVariable(_) => "unused_variable",
            Self::UnreachableCode => "unreachable_code",
            Self::EmptyIfBlock => "empty_if_block",
            Self::EmptyElseBlock => "empty_else_block",
            Self::EmptyWhileBody => "empty_while_body",
        }
    }
}

/// The list of possible labels on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum LintLabelKind {}

/// The list of possible notes on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum LintNoteKind {}

/// The list of possible helps on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum LintHelpKind {}

/// A Zircop lint
pub type LintDiagnostic =
    GenericDiagnostic<LintDiagnosticKind, LintLabelKind, LintNoteKind, LintHelpKind>;
