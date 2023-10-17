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

/// Converts the [`lexer::ZircoLexer`] result type of
/// [`Spanned<Result<Tok, LexicalError>>`] to something suitable to pass to
/// LALRPOP.
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

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;
    use crate::ast::expr::Expr;
    use crate::ast::ty::Type;

    mod expr {
        use super::*;

        #[test]
        fn arithmetic_operators_parse_as_expected() {
            assert_eq!(
                // ((1 + 1) - (((1 * 1) / 1) % 1))
                parse_expr("1 + 1 - 1 * 1 / 1 % 1"),
                Ok(Expr::sub(
                    Expr::add(
                        Expr::number(spanned!(0, "1".to_string(), 1)),
                        Expr::number(spanned!(4, "1".to_string(), 5))
                    ),
                    Expr::modulo(
                        Expr::div(
                            Expr::mul(
                                Expr::number(spanned!(8, "1".to_string(), 9)),
                                Expr::number(spanned!(12, "1".to_string(), 13))
                            ),
                            Expr::number(spanned!(16, "1".to_string(), 17))
                        ),
                        Expr::number(spanned!(20, "1".to_string(), 21))
                    )
                ))
            );
        }

        #[test]
        fn bitwise_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 & 1 | 1 ^ 1 << 1 >> 1"),
                Ok(Expr::bit_or(
                    Expr::bit_and(
                        Expr::number(spanned!(0, "1".to_string(), 1)),
                        Expr::number(spanned!(4, "1".to_string(), 5))
                    ),
                    Expr::bit_xor(
                        Expr::number(spanned!(8, "1".to_string(), 9)),
                        Expr::shr(
                            Expr::shl(
                                Expr::number(spanned!(12, "1".to_string(), 13)),
                                Expr::number(spanned!(17, "1".to_string(), 18))
                            ),
                            Expr::number(spanned!(22, "1".to_string(), 23))
                        )
                    )
                ))
            );
        }

        #[test]
        fn logical_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 && 1 || 1"),
                Ok(Expr::logical_or(
                    Expr::logical_and(
                        Expr::number(spanned!(0, "1".to_string(), 1)),
                        Expr::number(spanned!(5, "1".to_string(), 6))
                    ),
                    Expr::number(spanned!(10, "1".to_string(), 11))
                ))
            );
        }

        #[test]
        fn equality_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 == 1 != 1"),
                Ok(Expr::neq(
                    Expr::eq(
                        Expr::number(spanned!(0, "1".to_string(), 1)),
                        Expr::number(spanned!(5, "1".to_string(), 6))
                    ),
                    Expr::number(spanned!(10, "1".to_string(), 11))
                ))
            );
        }

        #[test]
        fn comparison_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 > 1 >= 1 < 1 <= 1"),
                Ok(Expr::lte(
                    Expr::lt(
                        Expr::gte(
                            Expr::gt(
                                Expr::number(spanned!(0, "1".to_string(), 1)),
                                Expr::number(spanned!(4, "1".to_string(), 5))
                            ),
                            Expr::number(spanned!(9, "1".to_string(), 10))
                        ),
                        Expr::number(spanned!(13, "1".to_string(), 14))
                    ),
                    Expr::number(spanned!(18, "1".to_string(), 19))
                ))
            );
        }

        #[test]
        fn unary_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("!~-&*x"),
                Ok(Expr::not(
                    Span::from_positions(0, 6),
                    Expr::bit_not(
                        Span::from_positions(1, 6),
                        Expr::neg(
                            Span::from_positions(2, 6),
                            Expr::address_of(
                                Span::from_positions(3, 6),
                                Expr::deref(
                                    Span::from_positions(4, 6),
                                    Expr::ident(spanned!(5, "x".to_string(), 6))
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
                Ok(Expr::arrow(
                    Expr::dot(
                        Expr::index(
                            Span::from_positions(0, 4),
                            Expr::ident(spanned!(0, "x".to_string(), 1)),
                            Expr::ident(spanned!(2, "x".to_string(), 3))
                        ),
                        spanned!(5, "x".to_string(), 6)
                    ),
                    spanned!(8, "x".to_string(), 9)
                ))
            );
        }

        #[test]
        fn function_calls_parse_as_expected() {
            assert_eq!(
                parse_expr("f(x, y)"),
                Ok(Expr::call(
                    Span::from_positions(0, 7),
                    Expr::ident(spanned!(0, "f".to_string(), 1)),
                    spanned!(
                        1,
                        vec![
                            Expr::ident(spanned!(2, "x".to_string(), 3)),
                            Expr::ident(spanned!(5, "y".to_string(), 6))
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
                Ok(Expr::ternary(
                    Expr::ident(spanned!(0, "a".to_string(), 1)),
                    Expr::ident(spanned!(4, "b".to_string(), 5)),
                    Expr::ident(spanned!(8, "c".to_string(), 9))
                ))
            );
        }

        #[test]
        fn casts_parse_as_expected() {
            assert_eq!(
                parse_expr("x as T"),
                Ok(Expr::cast(
                    Expr::ident(spanned!(0, "x".to_string(), 1)),
                    Type::ident(spanned!(5, "T".to_string(), 6))
                ))
            );
        }

        mod literals {
            use super::*;

            #[test]
            fn number_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("1"),
                    Ok(Expr::number(spanned!(0, "1".to_string(), 1)))
                );
            }

            #[test]
            fn string_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("\"x\""),
                    Ok(Expr::string(spanned!(0, "\"x\"".to_string(), 3)))
                );
            }

            #[test]
            fn identifiers_parse_as_expected() {
                assert_eq!(
                    parse_expr("x"),
                    Ok(Expr::ident(spanned!(0, "x".to_string(), 1)))
                );
            }

            #[test]
            fn booleans_parse_as_expected() {
                assert_eq!(parse_expr("true"), Ok(Expr::bool(spanned!(0, true, 4))));
            }
        }
    }
    mod program {}
}
