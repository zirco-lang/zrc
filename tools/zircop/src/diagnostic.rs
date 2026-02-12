//! Diagnostics producible by the Zircop tool

use thiserror::Error;
use zrc_diagnostics::diagnostic::{ErrorCode, GenericDiagnostic};

/// The list of possible lints Zircop can raise
#[expect(missing_docs)]
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum LintDiagnosticKind {
    #[error("empty struct used where `void` would be more appropriate")]
    EmptyStructUsed,
    #[error("variable with a '_' name has usages")]
    UnderscoreVariableUsed,
    #[error("unused variable")]
    UnusedVariable,
    #[error("unreachable code")]
    UnreachableCode,
    #[error("suspicious control flow")]
    SussyControlFlow,
    #[error("division by constant zero")]
    DivisionByConstantZero,
    #[error("unused statement")]
    UnusedStatement,
}
impl ErrorCode for LintDiagnosticKind {
    fn error_code(&self) -> &'static str {
        match self {
            Self::EmptyStructUsed => "empty_struct_used",
            Self::UnderscoreVariableUsed => "underscore_variable_used",
            Self::UnusedVariable => "unused_variable",
            Self::UnreachableCode => "unreachable_code",
            Self::SussyControlFlow => "suspicious_control_flow",
            Self::DivisionByConstantZero => "division_by_constant_zero",
            Self::UnusedStatement => "unused_statement",
        }
    }
}

/// The list of possible labels on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
#[expect(missing_docs)]
pub enum LintLabelKind {
    #[error("this `if` block is empty")]
    EmptyIf,
    #[error("but this `else` block has contents")]
    NonEmptyElse,
    #[error("this `else` block is empty")]
    EmptyElseBlock,
    #[error("this type is equivalent to `void`")]
    EmptyStructType,
    #[error("this variable is declared with a '_' prefix indicating it is unused")]
    UnderscoreVariableDeclaration,
    #[error("but it is used here")]
    UnderscoreVariableUsage,
    #[error("this variable is never used")]
    UnusedVariable,
    #[error("so this statement will never be executed")]
    UnreachableCode,
    #[error("this statement ends execution of this block")]
    PriorControlFlow,
    #[error("division by zero detected here")]
    DivisionByConstantZero,
    #[error("this statement has no side effects")]
    UnusedStatement,
}

/// The list of possible notes on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
#[expect(missing_docs)]
pub enum LintNoteKind {
    #[error("to suppress this warning, rename the variable to `{}`", .0)]
    UnusedVariableSuppress(String),
    #[error("this quotient may prove difficult to obtain")]
    DivisionByConstantZero,
    #[error("this expression appears to exist solely to troll the processor")]
    UnusedStatement,
}

/// The list of possible helps on Zircop lints
#[derive(Error, Debug, PartialEq, Eq, Clone)]
#[expect(missing_docs)]
pub enum LintHelpKind {
    #[error("consider inverting the condition: `if (!{}) ...`", .0)]
    InvertIfCondition(String),
    #[error("consider removing the `else` block")]
    RemoveElseBlock,
    #[error("consider using `void` instead")]
    UseVoidType,
    #[error("consider renaming the variable to `{}`", .0)]
    RenameVariable(String),
    #[error("consider removing the variable declaration if it has no side effects")]
    RemoveVariableDeclaration,
}

/// A Zircop lint
pub type LintDiagnostic =
    GenericDiagnostic<LintDiagnosticKind, LintLabelKind, LintNoteKind, LintHelpKind>;
