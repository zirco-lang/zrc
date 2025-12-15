//! Defines all possible Zirco compile time diagnostics.

use thiserror::Error;
use zrc_utils::span::{Span, Spannable};

use crate::{Diagnostic, diagnostic::ErrorCode};

/// The list of possible errors
///
/// Each variant represents a specific kind of diagnostic that can be
/// raised during the compilation process.
// These remain as Strings, not 'input str slices, to avoid circular dependencies (it's either use
// String, or use Tok, because str would be to-string()d from some tokens)
#[expect(missing_docs)]
#[derive(Error, Debug, PartialEq, Eq, Clone)]
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
    #[error("JavaScript user detected")]
    JavascriptUserDetected,

    // PARSER ERRORS
    /// Generic parser error
    #[error("invalid token")]
    InvalidToken,
    #[error("unexpected end of file, expected one of: {x}", x = .0.join(", "))]
    UnexpectedEof(Vec<String>),
    #[error("unrecognized token `{0}`, expected one of: {x}", x = .1.join(", "))]
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
    #[error("empty array literals are not allowed")]
    EmptyArrayLiteral,
    #[error("array element at index {index} has type `{found}`, expected `{expected}`")]
    ArrayElementTypeMismatch {
        expected: String,
        found: String,
        index: usize,
    },
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
    #[error("match on non-enum type `{0}`")]
    MatchOnNonEnum(String),
    #[error("there must be exactly one match arm for every enum variant")]
    MatchCaseCountMismatch,
    #[error("there must be a match arm for every enum variant")]
    NonExhaustiveMatchCases,
    #[error("main() function must have return type `i32`, got `{0}`")]
    MainFunctionMustReturnI32(String),
    #[error(
        "main() function may either have no parameters or two parameters, a `usize` and a `**u8`"
    )]
    MainFunctionInvalidParameters,
    #[error("cannot use constant `{0}` as an lvalue")]
    AssignmentToConstant(String),
    #[error("functions are not first-class values in Zirco; use a function pointer instead")]
    FunctionNotFirstClass,

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
    /// Create a [error] diagnostic in a given [`Span`].
    ///
    /// [error]: [`Severity::Error`]
    #[must_use]
    #[inline]
    pub fn error_in(self, span: Span) -> Diagnostic {
        Diagnostic::error(self.in_span(span))
    }
}
impl ErrorCode for DiagnosticKind {
    fn error_code(&self) -> &'static str {
        // 0xxx - (reserved for driver)
        // 1xxx - Preprocessor
        // 2xxx - Lexer and Parser
        // 3xxx - Typeck
        // 4xxx-9xxx - (reserved for future use)
        match self {
            Self::PreprocessorCannotDetermineParentDirectory(_) => "E1001",
            Self::PreprocessorCannotFindIncludeFile(_) => "E1002",
            Self::PreprocessorCannotReadIncludeFile(_, _) => "E1003",
            Self::PreprocessorInvalidIncludeSyntax => "E1004",
            Self::PreprocessorUnterminatedIncludeAngleBrackets => "E1005",
            Self::PreprocessorUnterminatedIncludeString => "E1006",
            Self::PreprocessorUnknownDirective(_) => "E1007",

            Self::UnknownToken(_) => "E2001",
            Self::UnterminatedStringLiteral => "E2002",
            Self::UnterminatedBlockComment => "E2003",
            Self::UnknownEscapeSequence => "E2004",
            Self::JavascriptUserDetected => "E2005",
            Self::InvalidToken => "E2006",
            Self::UnexpectedEof(_) => "E2101",
            Self::UnrecognizedToken(_, _) => "E2102",
            Self::ExtraToken(_) => "E2103",

            Self::UnableToResolveType(_) => "E3001",
            Self::UnableToResolveIdentifier(_) => "E3002",
            Self::NotAnLvalue(_) => "E3003",
            Self::InvalidAssignmentRightHandSideType { .. } => "E3004",
            Self::CannotDereferenceNonPointer(_) => "E3005",
            Self::CannotIndexIntoNonPointer(_) => "E3006",
            Self::StructOrUnionDoesNotHaveMember(_, _) => "E3007",
            Self::StructMemberAccessOnNonStruct(_) => "E3008",
            Self::FunctionArgumentCountMismatch { .. } => "E3009",
            Self::FunctionArgumentTypeMismatch { .. } => "E3010",
            Self::CannotCallNonFunction(_) => "E3011",
            Self::TernaryArmsMustHaveSameType(_, _) => "E3012",
            Self::ExpectedGot { .. } => "E3013",
            Self::ExpectedSameType(_, _) => "E3014",
            Self::EqualityOperators(_, _) => "E3015",
            Self::InvalidCast(_, _) => "E3016",
            Self::IdentifierAlreadyInUse(_) => "E3017",
            Self::NoTypeNoValue => "E3018",
            Self::EmptyArrayLiteral => "E3019",
            Self::ArrayElementTypeMismatch { .. } => "E3020",
            Self::CannotUseBreakOutsideOfLoop => "E3021",
            Self::CannotUseContinueOutsideOfLoop => "E3022",
            Self::CannotReturnHere => "E3023",
            Self::ExpectedABlockToReturn => "E3024",
            Self::DuplicateStructMember(_) => "E3025",
            Self::VariadicFunctionMustBeExternal => "E3026",
            Self::CannotDeclareVoid => "E3027",
            Self::InvalidPointerArithmeticOperation(_) => "E3028",
            Self::ConflictingFunctionDeclarations(_, _) => "E3029",
            Self::ConflictingImplementations(_) => "E3030",
            Self::InvalidNumberLiteralType(_) => "E3031",
            Self::SwitchCaseMissingTerminalDefault => "E3032",
            Self::MultipleCases => "E3033",
            Self::SelfReferentialTypeNotBehindPointer(_) => "E3034",
            Self::NumberLiteralOutOfBounds(_, _, _, _) => "E3035",
            Self::GlobalInitializerMustBeConstant => "E3036",
            Self::MatchOnNonEnum(_) => "E3037",
            Self::MatchCaseCountMismatch => "E3038",
            Self::NonExhaustiveMatchCases => "E3039",
            Self::MainFunctionMustReturnI32(_) => "E3040",
            Self::MainFunctionInvalidParameters => "E3041",
            Self::AssignmentToConstant(_) => "E3042",
            Self::FunctionNotFirstClass => "E3043",
        }
    }
}

/// The list of possible labels attached to a [`Diagnostic`]
#[derive(Debug, PartialEq, Eq, Clone, Error)]
#[expect(missing_docs)]
pub enum LabelKind {
    #[error("unknown token `{0}`")]
    UnknownToken(String),
    #[error("expected closing `*/`, got EOF")]
    UnterminatedBlockComment,
    #[error("block comment opened here")]
    BlockCommentOpenedHere,
    #[error("unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("unknown escape sequence")]
    UnknownEscapeSequence,
    #[error("JavaScript user detected (unknown token)")]
    JavascriptUserDetected,
}

/// The list of possible notes attached to a [`Diagnostic`]
#[expect(missing_docs)]
#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum NoteKind {
    #[error(
        "Zirco allows nested comments, so every opening `/*` must have a matching closing `*/`"
    )]
    NestedBlockComments,
}

/// The list of possible help messages attached to a [`Diagnostic`]
#[derive(Debug, PartialEq, Eq, Clone, Error)]
#[expect(missing_docs)]
pub enum HelpKind {
    #[error("did you mean `{0}`?")]
    JavascriptUserDetected(&'static str),
}
