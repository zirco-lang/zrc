// ! Parsing and parser errors
// !
// ! This module contains thin wrappers around the generated parser for the
// Zirco ! programming language, along with some additional models for error
// handling. !
// ! In most cases, you will be using the [`parse_program`] function to parse
// ! some input code. In some more specific situations, you may need to use
// ! [`parse_expr`] to parse a singular expression.
// !
// ! # Error handling
// ! The parser returns a [`Result`] that either yields the parsed
// ! [AST](super::ast) or a [`Diagnostic`]. For more information, read the
// ! documentation of [`Diagnostic`].

pub mod ty;
pub mod util;

use zrc_diagnostics::Diagnostic;
use zrc_utils::span::Spanned;

use crate::{
	ast::{
		expr::Expr,
		stmt::{Declaration, Stmt},
		ty::Type,
	},
	lexer::ZircoLexer,
};

/// A parser for Zirco source code.
#[derive(Debug)]
pub struct ZircoParser<'input> {
	/// The lexer that provides tokens to the parser
	lexer: ZircoLexer<'input>,
	/// The file being parsed
	file_name: &'static str,
}

impl<'input> ZircoParser<'input> {
	/// Creates a new parser for the given input and file name.
	#[must_use]
	pub const fn new(input: &'input str, file_name: &'static str, span_offset: usize) -> Self {
		Self {
			lexer: ZircoLexer::new(input, file_name, span_offset),
			file_name,
		}
	}
}

/// Parses a Zirco program with a specific file name, yielding a list of
/// [`Declaration`]s.
///
/// This function runs an **entire program** through the Zirco parser and
/// returns either a complete [AST](super::ast) consisting of root
/// [`Declaration`] nodes, or a list of [`Diagnostic`]s in the case of a
/// syntax error.
///
/// # Example
/// Obtaining the AST of a program:
/// ```
/// use zrc_parser::parser::parse_program;
/// let ast = parse_program("fn main() {}", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input program.
#[expect(clippy::result_large_err)]
pub fn parse_program<'input>(
	input: &'input str,
	file_name: &'static str,
) -> Result<Vec<Spanned<Declaration<'input>>>, Diagnostic> {
	todo!()
}

/// Parses a singular Zirco statement list, yielding a vector of AST [`Stmt`]
/// nodes.
///
/// This function only parses a single Zirco [statement](Stmt), and not an
/// entire program. Unless you are trying to do some special integration with
/// partial programs, you probably want to use the [`parse_program`] function
/// instead.
///
/// # Example
/// Obtaining the AST of a statement:
/// ```
/// use zrc_parser::parser::parse_stmt_list;
/// let ast = parse_stmt_list("let x = 6;", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input statement list.
#[expect(clippy::result_large_err)]
pub fn parse_stmt_list<'input>(
	input: &'input str,
	file_name: &'static str,
) -> Result<Spanned<Vec<Stmt<'input>>>, Diagnostic> {
	todo!()
}

/// Parses a singular Zirco type, yielding an AST [`Type`] node.
///
/// This function only parses a single Zirco [type](Type), and not an
/// entire program. Unless you are trying to do some special integration with
/// partial programs, you probably want to use the [`parse_program`] function
/// instead.
///
/// # Example
/// Obtaining the AST of a type:
/// ```
/// use zrc_parser::parser::parse_type;
/// let ast = parse_type("struct { x: i32 }", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input expression.
#[expect(clippy::result_large_err)]
pub fn parse_type<'input>(
	input: &'input str,
	file_name: &'static str,
) -> Result<Type<'input>, Diagnostic> {
	todo!()
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
/// let ast = parse_expr("1 + 2", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input expression.
#[expect(clippy::result_large_err)]
pub fn parse_expr<'input>(
	input: &'input str,
	file_name: &'static str,
) -> Result<Expr<'input>, Diagnostic> {
	todo!()
}

/// Parses a single source chunk from the preprocessor.
///
/// This function processes one source chunk and returns the parsed
/// declarations.
///
/// # Errors
/// This function returns [`Err`] with a diagnostic if any error was
/// encountered while parsing the chunk.
#[expect(clippy::result_large_err)]
pub fn parse_source_chunk(
	chunk: &zrc_preprocessor::SourceChunk,
) -> Result<Vec<Spanned<Declaration<'_>>>, Diagnostic> {
	todo!()
}
