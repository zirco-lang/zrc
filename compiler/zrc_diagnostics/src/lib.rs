#![doc=include_str!("../README.md")]
#![allow(unknown_lints)] // in case you use non-nightly clippy
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs,
    clippy::absolute_paths,
    clippy::as_conversions,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::deref_by_slicing,
    clippy::disallowed_script_idents,
    clippy::else_if_without_else,
    clippy::empty_structs_with_brackets,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::let_underscore_must_use,
    clippy::min_ident_chars,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::non_ascii_literal,
    clippy::redundant_type_annotations,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::unseparated_literal_suffix,
    clippy::string_to_string,
    clippy::todo,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unneeded_field_pattern,
    clippy::wildcard_enum_match_arm,
    let_underscore_drop,
    macro_use_extern_crate,
    missing_debug_implementations,
    non_exhaustive_omitted_patterns,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    variant_size_differences,
    unused_qualifications,
    clippy::unwrap_used,

    // These should be enabled in any non-user-facing code, like the parser, but not in the
    // frontend.
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    clippy::module_name_repetitions
)]

use std::{error::Error, fmt::Display};

use ansi_term::{Color, Style};
use line_span::LineSpanExt;
use thiserror::Error;
use zrc_utils::span::{Span, Spannable, Spanned};

/// The severity of a [`Diagnostic`].
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Severity {
    /// Error. Compilation will not continue.
    Error,
}
impl Severity {
    /// Get the display [`Style`] of this severity
    fn style(&self) -> Style {
        match *self {
            Self::Error => Color::Red.bold(),
        }
    }

    /// Get this severity's name
    const fn text(&self) -> &'static str {
        match *self {
            Self::Error => "error",
        }
    }
}
impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.style().paint(self.text()))
    }
}

/// A diagnostic message produced by zrc
#[derive(Debug, PartialEq, Eq)]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);
impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string
    #[must_use]
    pub fn print(&self, source: &str) -> String {
        format!(
            "{}: {}\n{}",
            self.0,
            Color::White.bold().paint(self.1.to_string()),
            display_source_window(&self.0, self.1.span(), source)
        )
    }
}
impl Error for Diagnostic {}
impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

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

/// Format and display the 'source window' -- the lines of span within str with
/// the underline where the span lies.
fn display_source_window(severity: &Severity, span: Span, source: &str) -> String {
    // First, we need to reduce source into only the lines we need to display. A
    // line should be displayed if *the line's span* intersects (see
    // MaybeIntersecting and intersect_spans) with the span we are trying to
    // display.

    // We can do this by iterating over the lines of source, and checking if the
    // line's span intersects with the span we are trying to display. If it does, we
    // add the line to a vector of lines to display.
    let lines = source
        .line_spans()
        .enumerate()
        .filter(|(_, line)| {
            Span::intersect(
                Span::from_positions(line.start(), line.ending()),
                Span::from_positions(span.start(), span.end()),
            )
            .is_some()
        })
        .map(|(n, line)| (n + 1, line))
        // lines now represents all of the lines we will eventually be formatting into the output.
        // we now need to find the span *within the line* that the span intersects with
        // we map each line to its string and intersecting span
        .map(|(n, line)| {
            (n, &source[line.start()..line.end()], {
                let intersection = Span::intersect(
                    Span::from_positions(line.start(), line.ending()),
                    Span::from_positions(span.start(), span.end()),
                )
                .expect("line span should intersect with span");
                (
                    intersection.start() - line.start(),
                    intersection.end() - line.start(),
                )
            })
        })
        .collect::<Vec<_>>();

    // Alright, cool. We now have an iterator over (line number, string, span within
    // string) which can be used to build our display. How much padding goes on
    // each line number?
    let max_line_number_length = lines
        .iter()
        .map(|(line, _, _)| line.to_string().len())
        .max()
        .expect("lines should not be empty")
        + 1; // i like the look of one extra character padding

    // Display format:
    // line | CODE CODE CODE CODE
    //      |      ^^^^
    // For every line in our input, we can generate both sides of it.
    lines
        .into_iter()
        .map(|(line_number, string, (start, end))| {
            format!(
                "{} {string}\n{} {} {}",
                Color::Blue
                    .bold()
                    .paint(format!("{line_number: >max_line_number_length$} |")),
                " ".repeat(max_line_number_length),
                Color::Blue.bold().paint("|"),
                severity
                    .style()
                    .paint(format!("{}{}", " ".repeat(start), "^".repeat(end - start)))
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// A trait to easily create [`Diagnostic`]s from [`Span`]s
/// See also: [`Spannable`]
pub trait SpanExt {
    /// Create a [`Diagnostic`] from this [`Span`] and a [`DiagnosticKind`]
    #[must_use]
    fn error(self, kind: DiagnosticKind) -> Diagnostic;
}
impl SpanExt for Span {
    #[inline]
    fn error(self, kind: DiagnosticKind) -> Diagnostic {
        Diagnostic(Severity::Error, kind.in_span(self))
    }
}

/// A trait to easily create [`Diagnostic`]s from [`Spanned`]s
/// See also: [`SpanExt`] and [`Spannable`]
pub trait SpannedExt<T> {
    /// Create a [`Diagnostic`] from this [`Spanned`] and a [`DiagnosticKind`]
    #[must_use]
    fn error(self, f: impl Fn(T) -> DiagnosticKind) -> Diagnostic;
}
impl<T> SpannedExt<T> for Spanned<T> {
    #[inline]
    fn error(self, f: impl Fn(T) -> DiagnosticKind) -> Diagnostic {
        Diagnostic(Severity::Error, self.map(f))
    }
}
