#![doc=include_str!("../README.md")]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::{error::Error, fmt::Display};

use ansi_term::{Color, Style};
use line_span::LineSpanExt;

/// A token with an associated span within the input.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Spanned<T>(pub usize, pub T, pub usize);

/// The severity of a [`Diagnostic`].
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Severity {
    /// Error. Compilation will not continue.
    Error,
}
impl Severity {
    /// Get the display [`Style`] of this severity
    fn style(&self) -> Style {
        match self {
            Self::Error => Color::Red.bold(),
        }
    }

    /// Get this severity's name
    const fn text(&self) -> &'static str {
        match self {
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
#[derive(Debug)]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);
impl Error for Diagnostic {}
impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1 .1)
    }
}

/// The list of possible errors
#[allow(missing_docs)]
#[derive(Debug)]
pub enum DiagnosticKind {
    // LEXER ERRORS
    UnknownToken(char),
    UnterminatedStringLiteral,
    UnterminatedBlockComment,

    // PARSER ERRORS
    /// Generic parser error
    InvalidToken,
    UnexpectedEof(Vec<String>),
    UnrecognizedToken(String, Vec<String>),
    ExtraToken(String),

    // TYPE CHECKER ERRORS
    UnableToResolveType(String),
    UnableToResolveIdentifier(String),
    AssignmentToNonPlace(String),
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
        expected: usize,
        got: usize,
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
}

impl Display for DiagnosticKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(c) => write!(f, "unknown token `{c}`"),
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

            Self::UnableToResolveType(t) => {
                write!(f, "unable to resolve `{t}` to a type")
            }
            Self::UnableToResolveIdentifier(i) => {
                write!(f, "unable to resolve identifier `{i}`")
            }
            Self::AssignmentToNonPlace(expr) => {
                write!(f, "cannot assign to non-place expression `{expr}`")
            }
            Self::InvalidAssignmentRightHandSideType { expected, got } => write!(
                f,
                "expected `{expected}` on right hand side of assignment, got `{got}`"
            ),
            Self::UnaryNotExpectedBoolean(t) => {
                write!(f, "expected boolean type, got `{t}`")
            }
            Self::UnaryBitwiseNotExpectedInteger(t) => {
                write!(f, "expected integer type, got `{t}`")
            }
            Self::UnaryMinusExpectedSignedInteger(t) => {
                write!(f, "expected signed integer type, got `{t}`")
            }
            Self::CannotDereferenceNonPointer(t) => {
                write!(f, "cannot dereference non-pointer type `{t}`")
            }
            Self::CannotIndexIntoNonPointer(t) => {
                write!(f, "cannot index into non-pointer type `{t}`")
            }
            Self::IndexOffsetMustBeInteger(t) => {
                write!(f, "index offset must be integer type, got `{t}`")
            }
            Self::StructDoesNotHaveMember(t, member) => {
                write!(f, "struct `{t}` does not have member `{member}`")
            }
            Self::StructMemberAccessOnNonStruct(t) => {
                write!(f, "cannot access member of non-struct type `{t}`")
            }
            Self::FunctionArgumentCountMismatch { expected, got } => {
                write!(f, "expected `{expected}` arguments, got `{got}`",)
            }
            Self::FunctionArgumentTypeMismatch { n, expected, got } => {
                write!(f, "expected `{expected}` for argument `{n}`, got `{got}`",)
            }
            Self::CannotCallNonFunction(t) => {
                write!(f, "cannot call non-function type `{t}`")
            }
            Self::TernaryConditionMustBeBoolean(t) => {
                write!(f, "ternary condition must be boolean type, got `{t}`")
            }
            Self::TernaryArmsMustHaveSameType(t1, t2) => {
                write!(f, "ternary arms must have same type, got `{t1}` and `{t2}`")
            }
            Self::ExpectedGot { expected, got } => {
                write!(f, "expected `{expected}`, got `{got}`")
            }
            Self::ExpectedSameType(t1, t2) => {
                write!(
                    f,
                    "expected both sides to have the same type, got `{t1}` and `{t2}`"
                )
            }
            Self::EqualityOperators(t1, t2) => write!(
                f,
                "expected both sides to be the same integer or pointer type, got `{t1}` and `{t2}`"
            ),
            Self::InvalidCast(from, to) => write!(f, "cannot cast `{from}` to `{to}`"),
            Self::IdentifierAlreadyInUse(i) => write!(f, "identifier `{i}` already in use"),
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
        }
    }
}

/// The intersection between two spans in the input
#[derive(PartialEq, Debug, Clone)]
enum MaybeIntersecting {
    /// There is no intersection
    Disjoint,
    /// There is an intersection and they may be equal
    Intersecting((usize, usize)),
}
impl MaybeIntersecting {
    /// Returns `true` if the spans intersect.
    const fn is_intersecting(&self) -> bool {
        self.intersection().is_some()
    }

    /// Returns an [`Option`] holding the intersection between the two spans, if
    /// it exists.
    const fn intersection(&self) -> Option<(usize, usize)> {
        match self {
            Self::Disjoint => None,
            Self::Intersecting((a, b)) => Some((*a, *b)),
        }
    }
}

/// Create a [`MaybeIntersecting`] from two spans
fn intersect_spans(a: (usize, usize), b: (usize, usize)) -> MaybeIntersecting {
    if a.0 > b.1 || b.0 > a.1 {
        MaybeIntersecting::Disjoint
    } else {
        MaybeIntersecting::Intersecting((std::cmp::max(a.0, b.0), std::cmp::min(a.1, b.1)))
    }
}

/// Format and display the 'source window' -- the lines of span within str with
/// the underline where the span lies.
fn display_source_window(severity: &Severity, span: (usize, usize), source: &str) -> String {
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
            intersect_spans(
                (line.start(), line.ending()),
                (span.0, span.1.saturating_sub(1)),
            )
            .is_intersecting()
        })
        .map(|(n, line)| (n + 1, line))
        // lines now represents all of the lines we will eventually be formatting into the output.
        // we now need to find the span *within the line* that the span intersects with
        // we map each line to its string and intersecting span
        .map(|(n, line)| {
            (n, &source[line.start()..line.end()], {
                let (s, e) = intersect_spans((line.start(), line.end()), (span.0, span.1))
                    .intersection()
                    .expect("line span should intersect with span");
                (s - line.start(), e - line.start())
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
            Color::White.bold().paint(self.1 .1.to_string()),
            display_source_window(&self.0.clone(), (self.1 .0, self.1 .2), source)
        )
    }
}
