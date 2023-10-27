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

    // These should be enabled in any non-user-facing code, like the parser, but not in the
    // frontend.
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::{error::Error, fmt::Display};

use ansi_term::{Color, Style};
use line_span::LineSpanExt;
use zrc_utils::span::{Span, Spanned};

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
impl Error for Diagnostic {}
impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1.value())
    }
}

/// The list of possible errors
// These remain as Strings, not 'input str slices, to avoid circular dependencies (it's either use
// String, or use Tok, because str would be to-string()d from some tokens)
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq)]
pub enum DiagnosticKind {
    // LEXER ERRORS
    UnknownToken(String),
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    UnknownEscapeSequence,

    // PARSER ERRORS
    /// Generic parser error
    InvalidToken,
    UnexpectedEof(Vec<String>),
    UnrecognizedToken(String, Vec<String>),
    ExtraToken(String),

    // TYPE CHECKER ERRORS
    UnableToResolveType(String),
    UnableToResolveIdentifier(String),
    NotAnLvalue(String),
    InvalidAssignmentRightHandSideType {
        expected: String,
        got: String,
    },
    UnaryNotExpectedBoolean(String),
    UnaryBitwiseNotExpectedInteger(String),
    UnaryMinusExpectedSignedInteger(String),
    CannotDereferenceNonPointer(String),
    CannotIndexIntoNonPointer(String),
    IndexOffsetMustBeInteger(String),
    StructDoesNotHaveMember(String, String),
    StructMemberAccessOnNonStruct(String),
    FunctionArgumentCountMismatch {
        expected: String,
        got: String,
    },
    FunctionArgumentTypeMismatch {
        n: usize, // counts from 0
        expected: String,
        got: String,
    },
    CannotCallNonFunction(String),
    TernaryConditionMustBeBoolean(String),
    TernaryArmsMustHaveSameType(String, String),
    ExpectedGot {
        expected: String,
        got: String,
    },
    ExpectedSameType(String, String),
    EqualityOperators(String, String),
    InvalidCast(String, String),
    IdentifierAlreadyInUse(String),
    NoTypeNoValue,
    CannotUseBreakOutsideOfLoop,
    CannotUseContinueOutsideOfLoop,
    CannotReturnHere,
    ExpectedABlockToReturn,
    DuplicateStructMember(String),
    VariadicFunctionMustBeExternal,
}

impl Display for DiagnosticKind {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(token) => write!(f, "unknown token `{token}`"),
            Self::UnterminatedStringLiteral => write!(f, "unterminated string literal"),
            Self::UnterminatedBlockComment => write!(f, "unterminated block comment"),

            Self::InvalidToken => write!(f, "invalid token"),
            Self::UnexpectedEof(expected) => {
                write!(
                    f,
                    "unexpected end of file, expected one of: {}",
                    expected.join(", ")
                )
            }
            Self::UnrecognizedToken(got, expected) => {
                write!(
                    f,
                    "unrecognized token `{got}`, expected one of: {}",
                    expected.join(", ")
                )
            }
            Self::ExtraToken(token) => write!(f, "extra token `{token}`"),

            Self::UnableToResolveType(ty) => {
                write!(f, "unable to resolve `{ty}` to a type")
            }
            Self::UnableToResolveIdentifier(i) => {
                write!(f, "unable to resolve identifier `{i}`")
            }
            Self::NotAnLvalue(expr) => {
                write!(
                    f,
                    "`{expr}` is not a valid lvalue for assignment or address-of"
                )
            }
            Self::InvalidAssignmentRightHandSideType { expected, got } => write!(
                f,
                "expected `{expected}` on right hand side of assignment, got `{got}`"
            ),
            Self::UnaryNotExpectedBoolean(ty) => {
                write!(f, "expected boolean type, got `{ty}`")
            }
            Self::UnaryBitwiseNotExpectedInteger(ty) => {
                write!(f, "expected integer type, got `{ty}`")
            }
            Self::UnaryMinusExpectedSignedInteger(ty) => {
                write!(f, "expected signed integer type, got `{ty}`")
            }
            Self::CannotDereferenceNonPointer(ty) => {
                write!(f, "cannot dereference non-pointer type `{ty}`")
            }
            Self::CannotIndexIntoNonPointer(ty) => {
                write!(f, "cannot index into non-pointer type `{ty}`")
            }
            Self::IndexOffsetMustBeInteger(ty) => {
                write!(f, "index offset must be integer type, got `{ty}`")
            }
            Self::StructDoesNotHaveMember(ty, member) => {
                write!(f, "struct `{ty}` does not have member `{member}`")
            }
            Self::StructMemberAccessOnNonStruct(ty) => {
                write!(f, "cannot access member of non-struct type `{ty}`")
            }
            Self::FunctionArgumentCountMismatch { expected, got } => {
                write!(f, "expected `{expected}` arguments, got `{got}`",)
            }
            Self::FunctionArgumentTypeMismatch { n, expected, got } => {
                write!(f, "expected `{expected}` for argument `{n}`, got `{got}`",)
            }
            Self::CannotCallNonFunction(ty) => {
                write!(f, "cannot call non-function type `{ty}`")
            }
            Self::TernaryConditionMustBeBoolean(ty) => {
                write!(f, "ternary condition must be boolean type, got `{ty}`")
            }
            Self::TernaryArmsMustHaveSameType(lhs_ty, rhs_ty) => {
                write!(
                    f,
                    "ternary arms must have same type, got `{lhs_ty}` and `{rhs_ty}`"
                )
            }
            Self::ExpectedGot { expected, got } => {
                write!(f, "expected `{expected}`, got `{got}`")
            }
            Self::ExpectedSameType(ty1, ty2) => {
                write!(
                    f,
                    "expected both sides to have the same type, got `{ty1}` and `{ty2}`"
                )
            }
            Self::EqualityOperators(ty1, ty2) => write!(
                f,
                concat!(
                    "expected both sides to be the same integer,",
                    " boolean or pointer type, got `{}` and `{}`"
                ),
                ty1, ty2
            ),
            Self::InvalidCast(from, to) => write!(f, "cannot cast `{from}` to `{to}`"),
            Self::IdentifierAlreadyInUse(id) => write!(f, "identifier `{id}` already in use"),
            Self::NoTypeNoValue => write!(
                f,
                "no explicit variable type present and no value to infer from"
            ),
            Self::CannotUseBreakOutsideOfLoop => write!(f, "cannot use `break` outside of loop"),
            Self::CannotUseContinueOutsideOfLoop => {
                write!(f, "cannot use `continue` outside of loop")
            }
            Self::CannotReturnHere => write!(f, "cannot use `return` here"),
            Self::ExpectedABlockToReturn => {
                write!(f, "expected a block to be guaranteed to return")
            }
            Self::DuplicateStructMember(key) => write!(f, "duplicate struct member `{key}`"),
            Self::VariadicFunctionMustBeExternal => write!(
                f,
                "cannot use variadic arguments (`...`) on a non-external function"
            ),
            Self::UnknownEscapeSequence => write!(f, "unknown escape sequence"),
        }
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
                Span::from_positions(span.start(), span.end() - 1),
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

impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string
    #[must_use]
    pub fn print(&self, source: &str) -> String {
        format!(
            "{}: {}\n{}",
            self.0,
            Color::White.bold().paint(self.1.value().to_string()),
            display_source_window(&self.0.clone(), self.1.span(), source)
        )
    }
}
