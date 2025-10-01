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
use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpannedExt};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    ast::{expr::Expr, stmt::Declaration},
    lexer,
};
use crate::{
    ast::{stmt::Stmt, ty::Type},
    internal_parser,
    lexer::LexicalError,
};

/// Converts from a LALRPOP [`ParseError`] to a corresponding [`Diagnostic`].
fn parser_error_to_diagnostic(
    error: ParseError<usize, lexer::Tok, Spanned<LexicalError>>,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location } => {
            DiagnosticKind::InvalidToken.error_in(Span::from_positions(location, location))
        }

        ParseError::UnrecognizedEof { location, expected } => {
            DiagnosticKind::UnexpectedEof(expected)
                .error_in(Span::from_positions(location - 1, location))
        }

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => DiagnosticKind::UnrecognizedToken(token.to_string(), expected)
            .error_in(Span::from_positions(start, end)),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => {
            DiagnosticKind::ExtraToken(token.to_string()).error_in(Span::from_positions(start, end))
        }

        ParseError::User { error } => error.error(|error| match error {
            LexicalError::UnknownToken(token) => DiagnosticKind::UnknownToken(token.to_string()),
            LexicalError::UnterminatedBlockComment => DiagnosticKind::UnterminatedBlockComment,
            LexicalError::UnterminatedStringLiteral => DiagnosticKind::UnterminatedStringLiteral,
            LexicalError::UnknownEscapeSequence => DiagnosticKind::UnknownEscapeSequence,
            LexicalError::JavascriptUserDetected(expected) => {
                DiagnosticKind::JavascriptUserDetected(expected)
            }
        }),
    }
}

/// Converts the [`lexer::ZircoLexer`] result type of
/// [`Spanned<Result<Tok, LexicalError>>`] to something suitable to pass to
/// LALRPOP.
fn zirco_lexer_span_to_lalrpop_span<'input>(
    spanned: Spanned<Result<lexer::Tok<'input>, LexicalError<'input>>>,
) -> Result<(usize, lexer::Tok<'input>, usize), Spanned<LexicalError<'input>>> {
    spanned.transpose().map(|spanned_tok| {
        let span = spanned_tok.span();
        (span.start(), spanned_tok.into_value(), span.end())
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
pub fn parse_program(input: &str) -> Result<Vec<Spanned<Declaration<'_>>>, Diagnostic> {
    internal_parser::ProgramParser::new()
        .parse(lexer::ZircoLexer::new(input).map(zirco_lexer_span_to_lalrpop_span))
        .map_err(parser_error_to_diagnostic)
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
/// let ast = parse_stmt_list("let x = 6;");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`ZircoParserError`] if any error was
/// encountered while parsing the input statement list.
pub fn parse_stmt_list(input: &str) -> Result<Spanned<Vec<Stmt<'_>>>, Diagnostic> {
    internal_parser::StmtListParser::new()
        .parse(lexer::ZircoLexer::new(input).map(zirco_lexer_span_to_lalrpop_span))
        .map(|stmt_list| stmt_list.in_span(Span::from_positions(0, input.len())))
        .map_err(parser_error_to_diagnostic)
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
/// let ast = parse_type("struct { x: i32 }");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`ZircoParserError`] if any error was
/// encountered while parsing the input expression.
pub fn parse_type(input: &str) -> Result<Type<'_>, Diagnostic> {
    internal_parser::TypeParser::new()
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
pub fn parse_expr(input: &str) -> Result<Expr<'_>, Diagnostic> {
    internal_parser::ExprParser::new()
        .parse(lexer::ZircoLexer::new(input).map(zirco_lexer_span_to_lalrpop_span))
        .map_err(parser_error_to_diagnostic)
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;
    use crate::ast::{expr::Expr, ty::Type};

    mod expr {
        use super::*;

        #[test]
        fn arithmetic_operators_parse_as_expected() {
            assert_eq!(
                // ((1 + 1) - (((1 * 1) / 1) % 1))
                parse_expr("1 + 1 - 1 * 1 / 1 % 1"),
                Ok(Expr::build_sub(
                    Expr::build_add(
                        Expr::build_number_dec(spanned!(0, "1", 1), None),
                        Expr::build_number_dec(spanned!(4, "1", 5), None)
                    ),
                    Expr::build_modulo(
                        Expr::build_div(
                            Expr::build_mul(
                                Expr::build_number_dec(spanned!(8, "1", 9), None),
                                Expr::build_number_dec(spanned!(12, "1", 13), None)
                            ),
                            Expr::build_number_dec(spanned!(16, "1", 17), None)
                        ),
                        Expr::build_number_dec(spanned!(20, "1", 21), None)
                    )
                ))
            );
        }

        #[test]
        fn bitwise_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 & 1 | 1 ^ 1 << 1 >> 1"),
                Ok(Expr::build_bit_or(
                    Expr::build_bit_and(
                        Expr::build_number_dec(spanned!(0, "1", 1), None),
                        Expr::build_number_dec(spanned!(4, "1", 5), None)
                    ),
                    Expr::build_bit_xor(
                        Expr::build_number_dec(spanned!(8, "1", 9), None),
                        Expr::build_shr(
                            Expr::build_shl(
                                Expr::build_number_dec(spanned!(12, "1", 13), None),
                                Expr::build_number_dec(spanned!(17, "1", 18), None)
                            ),
                            Expr::build_number_dec(spanned!(22, "1", 23), None)
                        )
                    )
                ))
            );
        }

        #[test]
        fn logical_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 && 1 || 1"),
                Ok(Expr::build_logical_or(
                    Expr::build_logical_and(
                        Expr::build_number_dec(spanned!(0, "1", 1), None),
                        Expr::build_number_dec(spanned!(5, "1", 6), None)
                    ),
                    Expr::build_number_dec(spanned!(10, "1", 11), None)
                ))
            );
        }

        #[test]
        fn equality_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 == 1 != 1"),
                Ok(Expr::build_neq(
                    Expr::build_eq(
                        Expr::build_number_dec(spanned!(0, "1", 1), None),
                        Expr::build_number_dec(spanned!(5, "1", 6), None)
                    ),
                    Expr::build_number_dec(spanned!(10, "1", 11), None)
                ))
            );
        }

        #[test]
        fn comparison_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 > 1 >= 1 < 1 <= 1"),
                Ok(Expr::build_lte(
                    Expr::build_lt(
                        Expr::build_gte(
                            Expr::build_gt(
                                Expr::build_number_dec(spanned!(0, "1", 1), None),
                                Expr::build_number_dec(spanned!(4, "1", 5), None)
                            ),
                            Expr::build_number_dec(spanned!(9, "1", 10), None)
                        ),
                        Expr::build_number_dec(spanned!(13, "1", 14), None)
                    ),
                    Expr::build_number_dec(spanned!(18, "1", 19), None)
                ))
            );
        }

        #[test]
        fn unary_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("!~-&*x"),
                Ok(Expr::build_not(
                    Span::from_positions(0, 6),
                    Expr::build_bit_not(
                        Span::from_positions(1, 6),
                        Expr::build_neg(
                            Span::from_positions(2, 6),
                            Expr::build_address_of(
                                Span::from_positions(3, 6),
                                Expr::build_deref(
                                    Span::from_positions(4, 6),
                                    Expr::build_ident(spanned!(5, "x", 6))
                                )
                            )
                        )
                    )
                ))
            );
        }

        #[test]
        fn indexing_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("x[x].x->x"),
                Ok(Expr::build_arrow(
                    Expr::build_dot(
                        Expr::build_index(
                            Span::from_positions(0, 4),
                            Expr::build_ident(spanned!(0, "x", 1)),
                            Expr::build_ident(spanned!(2, "x", 3))
                        ),
                        spanned!(5, "x", 6)
                    ),
                    spanned!(8, "x", 9)
                ))
            );
        }

        #[test]
        fn function_calls_parse_as_expected() {
            assert_eq!(
                parse_expr("f(x, y)"),
                Ok(Expr::build_call(
                    Span::from_positions(0, 7),
                    Expr::build_ident(spanned!(0, "f", 1)),
                    spanned!(
                        1,
                        vec![
                            Expr::build_ident(spanned!(2, "x", 3)),
                            Expr::build_ident(spanned!(5, "y", 6))
                        ],
                        7
                    )
                ))
            );
        }

        #[test]
        fn ternary_operator_parses_as_expected() {
            assert_eq!(
                parse_expr("a ? b : c"),
                Ok(Expr::build_ternary(
                    Expr::build_ident(spanned!(0, "a", 1)),
                    Expr::build_ident(spanned!(4, "b", 5)),
                    Expr::build_ident(spanned!(8, "c", 9))
                ))
            );
        }

        #[test]
        fn casts_parse_as_expected() {
            assert_eq!(
                parse_expr("x as T"),
                Ok(Expr::build_cast(
                    Expr::build_ident(spanned!(0, "x", 1)),
                    Type::build_ident(spanned!(5, "T", 6))
                ))
            );
        }

        mod literals {
            use super::*;
            use crate::lexer::{StringTok, ZrcString};

            #[test]
            fn number_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("1"),
                    Ok(Expr::build_number_dec(spanned!(0, "1", 1), None))
                );
            }

            #[test]
            fn string_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("\"x\""),
                    Ok(Expr::build_string(spanned!(
                        0,
                        ZrcString(vec![StringTok::Text("x")]),
                        3
                    )))
                );
            }

            #[test]
            fn identifiers_parse_as_expected() {
                assert_eq!(parse_expr("x"), Ok(Expr::build_ident(spanned!(0, "x", 1))));
            }

            #[test]
            fn booleans_parse_as_expected() {
                assert_eq!(
                    parse_expr("true"),
                    Ok(Expr::build_bool(spanned!(0, true, 4)))
                );
            }

            #[test]
            fn construction_syntax_parses_as_expected() {
                // Test empty struct construction
                let result = parse_expr("Point {}");
                assert!(result.is_ok(), "Failed to parse: {result:?}");
                assert_eq!(result.expect("should parse").to_string(), "(Point {  })");

                // Test struct construction with fields
                let result = parse_expr("Point { x: 1, y: 2 }");
                assert!(result.is_ok(), "Failed to parse: {result:?}");

                // Verify it stringifies correctly
                assert_eq!(
                    result.expect("should parse").to_string(),
                    "(Point { x: (1), y: (2) })"
                );
            }

            #[test]
            fn nested_construction_parses_as_expected() {
                let result = parse_expr("Outer { inner: Inner { x: 1 }, y: 2 }");
                assert!(
                    result.is_ok(),
                    "Failed to parse nested construction: {result:?}"
                );
            }
        }
    }

    mod ty {}

    mod stmt_list {}

    mod program {}
}
