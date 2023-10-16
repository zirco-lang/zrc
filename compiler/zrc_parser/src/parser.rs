//! Parsing and parser errors
//!
//! This module contains thin wrappers around the generated parser for the Zirco
//! programming language, along with some additional models for error handling.
//!
//! In most cases, you will be using the [`parse_program`] function to parse
//! some input code. In some more specific situations, you may need to use
//! [`parse_expr`] to parse a singular expression.
//!
//! # Error handling
//! The parser returns a [`Result`] that either yields the parsed
//! [AST](super::ast) or a [`ZircoParserError`]. For more information, read the
//! documentation of [`ZircoParserError`].
//!
//! # Example
//! For more examples, read the documentation for the corresponding parser
//! function.
//! ```
//! use zrc_parser::parser::parse_program;
//! let ast = parse_program("fn main() {}");
//! ```

use lalrpop_util::ParseError;
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_utils::span::{Span, Spanned};

use super::{
    ast::{expr::Expr, stmt::Declaration},
    lexer,
};
use crate::{internal_parser, lexer::LexicalError};

/// Converts from a LALRPOP [`ParseError`] to a corresponding [`Diagnostic`].
fn parser_error_to_diagnostic(
    error: ParseError<usize, lexer::Tok, Spanned<lexer::LexicalError>>,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location } => Diagnostic(
            Severity::Error,
            Span::from_positions(location, location).containing(DiagnosticKind::InvalidToken),
        ),

        ParseError::UnrecognizedEof { location, expected } => Diagnostic(
            Severity::Error,
            Span::from_positions(location - 1, location)
                .containing(DiagnosticKind::UnexpectedEof(expected)),
        ),

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => Diagnostic(
            Severity::Error,
            Span::from_positions(start, end).containing(DiagnosticKind::UnrecognizedToken(
                token.to_string(),
                expected,
            )),
        ),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => Diagnostic(
            Severity::Error,
            Span::from_positions(start, end)
                .containing(DiagnosticKind::ExtraToken(token.to_string())),
        ),

        ParseError::User { error } => {
            let span = error.span();
            match error.into_value() {
                LexicalError::UnknownToken(token) => Diagnostic(
                    Severity::Error,
                    span.containing(DiagnosticKind::UnknownToken(token)),
                ),
                LexicalError::UnterminatedBlockComment => Diagnostic(
                    Severity::Error,
                    span.containing(DiagnosticKind::UnterminatedBlockComment),
                ),
                LexicalError::UnterminatedStringLiteral => Diagnostic(
                    Severity::Error,
                    span.containing(DiagnosticKind::UnterminatedStringLiteral),
                ),
            }
        }
    }
}

/// Converts the [`lexer::ZircoLexer`] result type of [`Spanned<Result<Tok, LexicalError>>`] to something
/// suitable to pass to LALRPOP.
fn zirco_lexer_span_to_lalrpop_span(
    spanned: Spanned<Result<lexer::Tok, lexer::LexicalError>>,
) -> Result<(usize, lexer::Tok, usize), Spanned<lexer::LexicalError>> {
    spanned.transpose().map(|s| {
        let span = s.span();
        (span.start(), s.into_value(), span.end())
    })
}

/// Parses a Zirco program, yielding a list of [`Declaration`]s.
///
/// This function runs an **entire program** through the Zirco parser and
/// returns either a complete [AST](super::ast) consisting of root
/// [`Declaration`] nodes, or a list of [`ZircoParserError`]s in the case of a
/// syntax error.
///
/// # Example
/// Obtaining the AST of a program:
/// ```
/// use zrc_parser::parser::parse_program;
/// let ast = parse_program("fn main() {}");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`ZircoParserError`] if any error was
/// encountered while parsing the input program.
pub fn parse_program(input: &str) -> Result<Vec<Spanned<Declaration>>, Diagnostic> {
    internal_parser::ProgramParser::new()
        .parse(lexer::ZircoLexer::new(input).map(zirco_lexer_span_to_lalrpop_span))
        .map_err(parser_error_to_diagnostic)
}

/// Parses a singular Zirco expression, yielding an AST [`Expr`] node.
///
/// This function only parses a single Zirco [expression](Expr), and not an
/// entire program. Unless you are trying to do some special integration with
/// partial programs, you probably want to use the [`parse_program`] function
/// instead.
///
/// # Example
/// Obtaining the AST of an expression:
/// ```
/// use zrc_parser::parser::parse_expr;
/// let ast = parse_expr("1 + 2");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`ZircoParserError`] if any error was
/// encountered while parsing the input expression.
pub fn parse_expr(input: &str) -> Result<Expr, Diagnostic> {
    internal_parser::ExprParser::new()
        .parse(lexer::ZircoLexer::new(input).map(zirco_lexer_span_to_lalrpop_span))
        .map_err(parser_error_to_diagnostic)
}
