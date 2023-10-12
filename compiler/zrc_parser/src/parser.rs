//! Functions to parse Zirco into an Abstract Syntax Tree

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use lalrpop_util::{ErrorRecovery, ParseError};

use super::{
    ast::{
        expr::Expr,
        stmt::{Declaration, Stmt},
        Spanned,
    },
    lexer,
};
use crate::internal_parser;

/// An error returned from one of the Zirco parsing functions, like
/// [`parse_program`].
#[derive(Debug, PartialEq, Eq)]
pub enum ZircoParserError<T> {
    /// An error we were able to recover from
    Recoverable {
        /// The list of recoverable errors
        errors: Vec<ErrorRecovery<usize, lexer::Tok, lexer::LexicalError>>,
        /// The partial AST
        partial: T,
    },
    /// An error that stopped the parser.
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

/// More generic macro for [`parse_expr`] and [`parse_stmt`]
macro_rules! parse_internal {
    ($parser: ty, $input: expr) => {{
        let mut errors = Vec::new();
        let result = <$parser>::new().parse(&mut errors, lexer::ZircoLexer::new($input));
        match result {
            Err(e) => Err(ZircoParserError::Fatal(e)),
            Ok(v) if errors.is_empty() => Ok(v),
            Ok(v) => Err(ZircoParserError::Recoverable { errors, partial: v }),
        }
    }};
}

/// Parse a program with the Zirco parser.
///
/// # Errors
/// Returns a valid [`ZircoParserError`] if parsing fails.
pub fn parse_program(
    input: &str,
) -> Result<Vec<Spanned<Declaration>>, ZircoParserError<Vec<Spanned<Declaration>>>> {
    parse_internal!(internal_parser::ProgramParser, input)
}

/// Parse a string as an expression with the Zirco parser.
///
/// # Errors
/// Returns a valid [`ZircoParserError`] if parsing fails.
pub fn parse_expr(input: &str) -> Result<Expr, ZircoParserError<Expr>> {
    parse_internal!(internal_parser::ExprParser, input)
}

/// Parse a string as a statement with the Zirco parser.
///
/// # Errors
/// Returns a valid [`ZircoParserError`] if parsing fails.
pub fn parse_stmt(input: &str) -> Result<Stmt, ZircoParserError<Stmt>> {
    parse_internal!(internal_parser::StmtParser, input)
}
