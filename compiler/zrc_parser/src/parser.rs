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
//! function. ```
//! use zrc_parser::parser::parse_program;
//! let ast = parse_program("fn main() {}");
//! ```

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use lalrpop_util::{ErrorRecovery, ParseError};
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity, Spanned as DiagnosticSpan};

use super::{
    ast::{expr::Expr, stmt::Declaration, Spanned},
    lexer,
};
use crate::internal_parser;

/// Representation of a parser error that may have returned a partial AST.
///
/// In the Zirco parser, we are capable of recovering from some errors. This
/// means that the parser may still return an AST in some cases even if a syntax
/// error was encountered. This is useful for IDEs and other tools that want to
/// provide syntax highlighting or other features that require at least a
/// partial AST.
///
/// When you receive the [`ZircoParserError::Recoverable`] variant, you can
/// access the [`partial`](ZircoParserError::Recoverable::partial) field to
/// obtain the partial AST. This AST is "partial" in the way that it may contain
/// some `Error` tokens, such as
/// [`ExprKind::Error`](super::ast::expr::ExprKind::Error).
///
/// In the case of the [`ZircoParserError::Fatal`] variant, you cannot access a
/// partial AST as none was able to be recovered, and your application must just
/// handle the corresponding LALRPOP [`ParseError`].
///
/// This type is often found wrapped in a [`Result`] type, where the [`Ok`]
/// variant contains the full AST and the [`Err`] variant contains a
/// [`ZircoParserError`].
#[derive(Debug, PartialEq, Eq)]
pub enum ZircoParserError<T> {
    /// The parser encountered an error, but was still able to produce a partial
    /// AST. This AST may contain some `Error` tokens, such as
    /// [`ExprKind::Error`](super::ast::expr::ExprKind::Error).
    Recoverable {
        /// The list of [`ErrorRecovery`] instances corresponding with the
        /// errors that were encountered during parsing.
        errors: Vec<ErrorRecovery<usize, lexer::Tok, lexer::LexicalError>>,
        /// The partial AST that was produced by the parser. This AST may
        /// contain some `Error` tokens,
        /// such as [`ExprKind::Error`](super::ast::expr::ExprKind::Error).
        partial: T,
    },
    /// The parser encountered an error, and was unable to produce a partial
    /// AST.
    Fatal(ParseError<usize, lexer::Tok, lexer::LexicalError>),
}
impl<T: Debug> Error for ZircoParserError<T> {}
impl<T: Debug> Display for ZircoParserError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Recoverable { errors, .. } => {
                write!(f, "Recoverable parser error: ")?;
                for error in errors {
                    writeln!(f, "{}", error.error)?;
                }
                Ok(())
            }
            Self::Fatal(e) => write!(f, "Fatal parser error: {e}"),
        }
    }
}

/// Converts from a LALRPOP [`ParseError`] to a corresponding [`Diagnostic`].
fn parser_error_to_diagnostic(
    error: ParseError<usize, lexer::Tok, lexer::LexicalError>,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(location, DiagnosticKind::InvalidToken, location),
        ),

        ParseError::UnrecognizedEof { location, expected } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(
                location - 1,
                DiagnosticKind::UnexpectedEof(expected),
                location,
            ),
        ),

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(
                start,
                DiagnosticKind::UnrecognizedToken(token.to_string(), expected),
                end,
            ),
        ),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(start, DiagnosticKind::ExtraToken(token.to_string()), end),
        ),

        ParseError::User {
            error: lexer::LexicalError::UnknownToken(start, tok, end),
        } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(start, DiagnosticKind::UnknownToken(tok), end),
        ),

        ParseError::User {
            error: lexer::LexicalError::UnterminatedBlockComment(start, end),
        } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(start, DiagnosticKind::UnterminatedBlockComment, end),
        ),

        ParseError::User {
            error: lexer::LexicalError::UnterminatedStringLiteral(start, end),
        } => Diagnostic(
            Severity::Error,
            DiagnosticSpan(start, DiagnosticKind::UnterminatedStringLiteral, end),
        ),
    }
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
        .parse(lexer::ZircoLexer::new(input))
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
        .parse(lexer::ZircoLexer::new(input))
        .map_err(parser_error_to_diagnostic)
}
