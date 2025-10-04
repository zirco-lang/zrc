//! Defines all possible Zirco compile time diagnostics.

use thiserror::Error;
use zrc_utils::span::{Span, Spannable};

use crate::{Diagnostic, Severity};

/// The list of possible errors
// These remain as Strings, not 'input str slices, to avoid circular dependencies (it's either use
// String, or use Tok, because str would be to-string()d from some tokens)
#[allow(missing_docs)]
#[derive(Error, Debug, PartialEq, Eq)]
pub enum DiagnosticKind {
    // LEXER ERRORS
    #[error("unknown token `{0}`")]
    UnknownToken(String),
    #[error("unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("unterminated block comment")]
    UnterminatedBlockComment,
    #[error("unknown escape sequence")]
    UnknownEscapeSequence,
    /// Raised if `===` or `!==` is found in the input.
    /// Parameter will be either `"=="` or `"!="` for what was expected.
    #[error("JavaScript user detected -- did you mean `{0}`?")]
    JavascriptUserDetected(&'static str),

    // PARSER ERRORS
    /// Generic parser error
    #[error("invalid token")]
    InvalidToken,
    #[error("unexpected end of file, expected one of: {}", .0.join(", "))]
    UnexpectedEof(Vec<String>),
    #[error("unrecognized token `{0}`, expected one of: {}", .1.join(", "))]
    UnrecognizedToken(String, Vec<String>),
    #[error("extra token `{0}`")]
    ExtraToken(String),

    // TYPE CHECKER ERRORS
    #[error("unable to resolve `{0}` to a type")]
    UnableToResolveType(String),
    #[error("unable to resolve identifier `{0}`")]
    UnableToResolveIdentifier(String),
    #[error("`{0}` is not a valid lvalue for assignment or address-of")]
    NotAnLvalue(String),
    #[error("expected `{expected}` on right hand side of assignment, got `{got}`")]
    InvalidAssignmentRightHandSideType { expected: String, got: String },
    #[error("cannot dereference non-pointer type `{0}`")]
    CannotDereferenceNonPointer(String),
    #[error("cannot index into non-pointer type `{0}`")]
    CannotIndexIntoNonPointer(String),
    #[error("`{0}` does not have member `{1}`")]
    StructOrUnionDoesNotHaveMember(String, String),
    #[error("cannot access member of non-struct type `{0}`")]
    StructMemberAccessOnNonStruct(String),
    #[error("expected `{expected}` arguments, got `{got}`")]
    FunctionArgumentCountMismatch { expected: String, got: String },
    #[error("expected `{expected}` for argument `{n}`, got `{got}`")]
    FunctionArgumentTypeMismatch {
        n: usize, // counts from 0
        expected: String,
        got: String,
    },
    #[error("cannot call non-function type `{0}`")]
    CannotCallNonFunction(String),
    #[error("ternary arms must have same type, got `{0}` and `{1}`")]
    TernaryArmsMustHaveSameType(String, String),
    #[error("expected `{expected}`, got `{got}`")]
    ExpectedGot { expected: String, got: String },
    #[error("expected both sides to have the same type, got `{0}` and `{1}`")]
    ExpectedSameType(String, String),
    #[error(
        "expected both sides to be the same integer, boolean or pointer type, got `{0}` and `{1}`"
    )]
    EqualityOperators(String, String),
    #[error("cannot cast `{0}` to `{1}`")]
    InvalidCast(String, String),
    #[error("identifier `{0}` already in use")]
    IdentifierAlreadyInUse(String),
    #[error("no explicit variable type present and no value to infer from")]
    NoTypeNoValue,
    #[error("cannot use `break` outside of loop")]
    CannotUseBreakOutsideOfLoop,
    #[error("cannot use `continue` outside of loop")]
    CannotUseContinueOutsideOfLoop,
    #[error("cannot use `return` here")]
    CannotReturnHere,
    #[error("expected a block to be guaranteed to return")]
    ExpectedABlockToReturn,
    #[error("duplicate struct member `{0}`")]
    DuplicateStructMember(String),
    #[error("cannot use variadic arguments (`...`) on a non-external function")]
    VariadicFunctionMustBeExternal,
    #[error("cannot declare a variable of type `void` -- just discard the value")]
    CannotDeclareVoid,
    #[error("invalid pointer arithmetic operation `{0}`")]
    InvalidPointerArithmeticOperation(String),
    #[error("declaration of type `{1}` conflicts with previous declaration with type `{0}`")]
    ConflictingFunctionDeclarations(String, String),
    #[error("function {0} has multiple implementations in this unit")]
    ConflictingImplementations(String),
    #[error("type {0} cannot be used for number literals")]
    InvalidNumberLiteralType(String),
    #[error("a switch case statement must always end in a default block")]
    SwitchCaseMissingTerminalDefault,
    #[error("multiple case statements are matching for the same value")]
    MultipleCases,
    #[error("self-referential type `{0}` must be behind a pointer (e.g., `*{0}`)")]
    SelfReferentialTypeNotBehindPointer(String),
    #[error("number literal `{0}` is out of bounds for type `{1}` (valid range: {2} to {3})")]
    NumberLiteralOutOfBounds(String, String, String, String),
    #[error("global variable initializer must be a constant expression")]
    GlobalInitializerMustBeConstant,

    // PREPROCESSOR ERRORS
    #[error("unterminated include string")]
    PreprocessorUnterminatedIncludeString,
    #[error("unterminated include angle brackets")]
    PreprocessorUnterminatedIncludeAngleBrackets,
    #[error("include directive must use \"file\" or <file> syntax")]
    PreprocessorInvalidIncludeSyntax,
    #[error("cannot find include file: {0}")]
    PreprocessorCannotFindIncludeFile(String),
    #[error("cannot read include file {0}: {1}")]
    PreprocessorCannotReadIncludeFile(String, String),
    #[error("cannot determine parent directory for {0}")]
    PreprocessorCannotDetermineParentDirectory(String),
    #[error("unknown preprocessor directive: #{0}")]
    PreprocessorUnknownDirective(String),
}
impl DiagnosticKind {
    /// Create an [error] diagnostic in a given [`Span`]
    ///
    /// [error]: [`Severity::Error`]
    #[must_use]
    #[inline]
    pub fn error_in(self, span: Span) -> Diagnostic {
        Diagnostic(Severity::Error, self.in_span(span))
    }
}
