#![doc=include_str!("../README.md")]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use line_span::LineSpanExt;
use std::{error::Error, fmt::Display};

use ansi_term::{Color, Style};

/// A token with an associated span within the input.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Spanned<T>(pub usize, pub T, pub usize);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Severity {
    Error,
}
impl Severity {
    fn style(&self) -> Style {
        match self {
            Self::Error => Color::Red.bold(),
        }
    }

    fn text(&self) -> &'static str {
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

#[derive(Debug)]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);
impl Error for Diagnostic {}
impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1 .1.to_string())
    }
}

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
            DiagnosticKind::UnknownToken(c) => write!(f, "unknown token `{c}`"),
            DiagnosticKind::UnterminatedStringLiteral => write!(f, "unterminated string literal"),
            DiagnosticKind::UnterminatedBlockComment => write!(f, "unterminated block comment"),

            DiagnosticKind::InvalidToken => write!(f, "invalid token"),
            DiagnosticKind::UnexpectedEof(expected) => {
                write!(
                    f,
                    "unexpected end of file, expected one of: {}",
                    expected.join(", ")
                )
            }
            DiagnosticKind::UnrecognizedToken(got, expected) => {
                write!(
                    f,
                    "unrecognized token `{got}`, expected one of: {}",
                    expected.join(", ")
                )
            }
            DiagnosticKind::ExtraToken(token) => write!(f, "extra token `{}`", token),

            DiagnosticKind::UnableToResolveType(t) => {
                write!(f, "unable to resolve `{t}` to a type")
            }
            DiagnosticKind::UnableToResolveIdentifier(i) => {
                write!(f, "unable to resolve identifier `{i}`")
            }
            DiagnosticKind::AssignmentToNonPlace(expr) => {
                write!(f, "cannot assign to non-place expression `{expr}`")
            }
            DiagnosticKind::InvalidAssignmentRightHandSideType { expected, got } => write!(
                f,
                "expected `{expected}` on right hand side of assignment, got `{got}`"
            ),
            DiagnosticKind::UnaryNotExpectedBoolean(t) => {
                write!(f, "expected boolean type, got `{t}`")
            }
            DiagnosticKind::UnaryBitwiseNotExpectedInteger(t) => {
                write!(f, "expected integer type, got `{t}`")
            }
            DiagnosticKind::UnaryMinusExpectedSignedInteger(t) => {
                write!(f, "expected signed integer type, got `{t}`")
            }
            DiagnosticKind::CannotDereferenceNonPointer(t) => {
                write!(f, "cannot dereference non-pointer type `{t}`")
            }
            DiagnosticKind::CannotIndexIntoNonPointer(t) => {
                write!(f, "cannot index into non-pointer type `{t}`")
            }
            DiagnosticKind::IndexOffsetMustBeInteger(t) => {
                write!(f, "index offset must be integer type, got `{t}`")
            }
            DiagnosticKind::StructDoesNotHaveMember(t, member) => {
                write!(f, "struct `{t}` does not have member `{member}`")
            }
            DiagnosticKind::StructMemberAccessOnNonStruct(t) => {
                write!(f, "cannot access member of non-struct type `{t}`")
            }
            DiagnosticKind::FunctionArgumentCountMismatch { expected, got } => {
                write!(f, "expected `{expected}` arguments, got `{got}`",)
            }
            DiagnosticKind::FunctionArgumentTypeMismatch { n, expected, got } => {
                write!(f, "expected `{expected}` for argument `{n}`, got `{got}`",)
            }
            DiagnosticKind::CannotCallNonFunction(t) => {
                write!(f, "cannot call non-function type `{t}`")
            }
            DiagnosticKind::TernaryConditionMustBeBoolean(t) => {
                write!(f, "ternary condition must be boolean type, got `{t}`")
            }
            DiagnosticKind::TernaryArmsMustHaveSameType(t1, t2) => {
                write!(f, "ternary arms must have same type, got `{t1}` and `{t2}`")
            }
            DiagnosticKind::ExpectedGot { expected, got } => {
                write!(f, "expected `{expected}`, got `{got}`")
            }
            DiagnosticKind::ExpectedSameType(t1, t2) => {
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

#[derive(PartialEq, Debug, Clone)]
enum MaybeIntersecting {
    Disjoint,
    /// May be equal
    Intersecting((usize, usize)),
}
impl MaybeIntersecting {
    fn is_disjoint(&self) -> bool {
        matches!(self, MaybeIntersecting::Disjoint)
    }

    fn is_intersecting(&self) -> bool {
        matches!(self, MaybeIntersecting::Intersecting(_))
    }

    fn intersection(&self) -> Option<(usize, usize)> {
        match self {
            MaybeIntersecting::Disjoint => None,
            MaybeIntersecting::Intersecting((a, b)) => Some((*a, *b)),
        }
    }
}
fn intersect_spans(a: (usize, usize), b: (usize, usize)) -> MaybeIntersecting {
    if a.0 > b.1 || b.0 > a.1 {
        MaybeIntersecting::Disjoint
    } else {
        MaybeIntersecting::Intersecting((std::cmp::max(a.0, b.0), std::cmp::min(a.1, b.1)))
    }
}

/// Format and display the 'source window' -- the lines of span within str with the underline where the span lies.
fn display_source_window(severity: Severity, span: (usize, usize), source: &str) -> String {
    // First, we need to reduce source into only the lines we need to display. A line should be displayed if *the line's span* intersects (see MaybeIntersecting and intersect_spans) with the span we are trying to display.

    // We can do this by iterating over the lines of source, and checking if the line's span intersects with the span we are trying to display. If it does, we add the line to a vector of lines to display.
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
        // lines now represents all of the lines we will eventually be formatting into the output. we now need to find the span *within the line* that the span intersects with
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

    // Alright, cool. We now have an iterator over (line number, string, span within string) which can be used to build our display.
    // How much padding goes on each line number?
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
    pub fn print(&self, source: &str) -> String {
        format!(
            "{}: {}\n{}",
            self.0,
            Color::White.bold().paint(self.1 .1.to_string()),
            display_source_window(self.0.clone(), (self.1 .0, self.1 .2), source)
        )
    }
}
