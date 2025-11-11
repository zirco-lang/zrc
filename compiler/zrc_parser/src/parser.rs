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
//! [AST](super::ast) or a [`Diagnostic`]. For more information, read the
//! documentation of [`Diagnostic`].
//!
//! # Example
//! For more examples, read the documentation for the corresponding parser
//! function.
//! ```
//! use zrc_parser::parser::parse_program;
//! let ast = parse_program("fn main() {}", "<test>");
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
        ParseError::InvalidToken { location } => DiagnosticKind::InvalidToken.error_in(
            Span::from_positions_and_file(location, location, "<unknown>"),
        ),

        ParseError::UnrecognizedEof { location, expected } => {
            DiagnosticKind::UnexpectedEof(expected).error_in(Span::from_positions_and_file(
                location - 1,
                location,
                "<unknown>",
            ))
        }

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => DiagnosticKind::UnrecognizedToken(token.to_string(), expected)
            .error_in(Span::from_positions_and_file(start, end, "<unknown>")),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => DiagnosticKind::ExtraToken(token.to_string()).error_in(Span::from_positions_and_file(
            start,
            end,
            "<unknown>",
        )),

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
#[allow(clippy::result_large_err)]
pub fn parse_program<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Vec<Spanned<Declaration<'input>>>, Diagnostic> {
    internal_parser::ProgramParser::new()
        .parse(
            file_name,
            lexer::ZircoLexer::new(input, file_name).map(zirco_lexer_span_to_lalrpop_span),
        )
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
/// let ast = parse_stmt_list("let x = 6;", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input statement list.
#[allow(clippy::result_large_err)]
pub fn parse_stmt_list<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Spanned<Vec<Stmt<'input>>>, Diagnostic> {
    internal_parser::StmtListParser::new()
        .parse(
            file_name,
            lexer::ZircoLexer::new(input, file_name).map(zirco_lexer_span_to_lalrpop_span),
        )
        .map(|stmt_list| {
            stmt_list.in_span(Span::from_positions_and_file(0, input.len(), file_name))
        })
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
/// let ast = parse_type("struct { x: i32 }", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input expression.
#[allow(clippy::result_large_err)]
pub fn parse_type<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Type<'input>, Diagnostic> {
    internal_parser::TypeParser::new()
        .parse(
            file_name,
            lexer::ZircoLexer::new(input, file_name).map(zirco_lexer_span_to_lalrpop_span),
        )
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
/// let ast = parse_expr("1 + 2", "<test>");
/// ```
///
/// # Errors
/// This function returns [`Err`] with a [`Diagnostic`] if any error was
/// encountered while parsing the input expression.
#[allow(clippy::result_large_err)]
pub fn parse_expr<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Expr<'input>, Diagnostic> {
    internal_parser::ExprParser::new()
        .parse(
            file_name,
            lexer::ZircoLexer::new(input, file_name).map(zirco_lexer_span_to_lalrpop_span),
        )
        .map_err(parser_error_to_diagnostic)
}

/// Parses a single source chunk from the preprocessor.
///
/// This function processes one source chunk and returns the parsed
/// declarations.
///
/// # Errors
/// This function returns [`Err`] with a diagnostic if any error was
/// encountered while parsing the chunk.
#[allow(clippy::result_large_err)]
pub fn parse_source_chunk(
    chunk: &zrc_preprocessor::SourceChunk,
) -> Result<Vec<Spanned<Declaration<'_>>>, Diagnostic> {
    // Convert String to &'static str using Box::leak
    let file_name: &'static str = Box::leak(chunk.file_name.clone().into_boxed_str());

    internal_parser::ProgramParser::new()
        .parse(
            file_name,
            lexer::ZircoLexer::new(&chunk.content, file_name).map(zirco_lexer_span_to_lalrpop_span),
        )
        .map_err(parser_error_to_diagnostic)
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned_test;

    use super::*;
    use crate::ast::{expr::Expr, ty::Type};

    mod expr {
        use super::*;

        #[test]
        fn arithmetic_operators_parse_as_expected() {
            assert_eq!(
                // ((1 + 1) - (((1 * 1) / 1) % 1))
                parse_expr("1 + 1 - 1 * 1 / 1 % 1", "<test>"),
                Ok(Expr::build_sub(
                    Expr::build_add(
                        Expr::build_number_dec(spanned_test!(0, "1", 1), None),
                        Expr::build_number_dec(spanned_test!(4, "1", 5), None)
                    ),
                    Expr::build_modulo(
                        Expr::build_div(
                            Expr::build_mul(
                                Expr::build_number_dec(spanned_test!(8, "1", 9), None),
                                Expr::build_number_dec(spanned_test!(12, "1", 13), None)
                            ),
                            Expr::build_number_dec(spanned_test!(16, "1", 17), None)
                        ),
                        Expr::build_number_dec(spanned_test!(20, "1", 21), None)
                    )
                ))
            );
        }

        #[test]
        fn bitwise_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 & 1 | 1 ^ 1 << 1 >> 1", "<test>"),
                Ok(Expr::build_bit_or(
                    Expr::build_bit_and(
                        Expr::build_number_dec(spanned_test!(0, "1", 1), None),
                        Expr::build_number_dec(spanned_test!(4, "1", 5), None)
                    ),
                    Expr::build_bit_xor(
                        Expr::build_number_dec(spanned_test!(8, "1", 9), None),
                        Expr::build_shr(
                            Expr::build_shl(
                                Expr::build_number_dec(spanned_test!(12, "1", 13), None),
                                Expr::build_number_dec(spanned_test!(17, "1", 18), None)
                            ),
                            Expr::build_number_dec(spanned_test!(22, "1", 23), None)
                        )
                    )
                ))
            );
        }

        #[test]
        fn pipe_operator_parses_as_expected() {
            // Test basic pipe: a |> f
            assert_eq!(
                parse_expr("a |> f", "<test>"),
                Ok(Expr::build_pipe(
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(5, "f", 6))
                ))
            );

            // Test chained pipe: a |> f |> g (left-associative)
            assert_eq!(
                parse_expr("a |> f |> g", "<test>"),
                Ok(Expr::build_pipe(
                    Expr::build_pipe(
                        Expr::build_ident(spanned_test!(0, "a", 1)),
                        Expr::build_ident(spanned_test!(5, "f", 6))
                    ),
                    Expr::build_ident(spanned_test!(10, "g", 11))
                ))
            );

            // Test pipe with precedence same as bitwise OR
            assert_eq!(
                parse_expr("a | b |> c", "<test>"),
                Ok(Expr::build_pipe(
                    Expr::build_bit_or(
                        Expr::build_ident(spanned_test!(0, "a", 1)),
                        Expr::build_ident(spanned_test!(4, "b", 5))
                    ),
                    Expr::build_ident(spanned_test!(9, "c", 10))
                ))
            );
        }

        #[test]
        fn logical_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 && 1 || 1", "<test>"),
                Ok(Expr::build_logical_or(
                    Expr::build_logical_and(
                        Expr::build_number_dec(spanned_test!(0, "1", 1), None),
                        Expr::build_number_dec(spanned_test!(5, "1", 6), None)
                    ),
                    Expr::build_number_dec(spanned_test!(10, "1", 11), None)
                ))
            );
        }

        #[test]
        fn equality_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 == 1 != 1", "<test>"),
                Ok(Expr::build_neq(
                    Expr::build_eq(
                        Expr::build_number_dec(spanned_test!(0, "1", 1), None),
                        Expr::build_number_dec(spanned_test!(5, "1", 6), None)
                    ),
                    Expr::build_number_dec(spanned_test!(10, "1", 11), None)
                ))
            );
        }

        #[test]
        fn comparison_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("1 > 1 >= 1 < 1 <= 1", "<test>"),
                Ok(Expr::build_lte(
                    Expr::build_lt(
                        Expr::build_gte(
                            Expr::build_gt(
                                Expr::build_number_dec(spanned_test!(0, "1", 1), None),
                                Expr::build_number_dec(spanned_test!(4, "1", 5), None)
                            ),
                            Expr::build_number_dec(spanned_test!(9, "1", 10), None)
                        ),
                        Expr::build_number_dec(spanned_test!(13, "1", 14), None)
                    ),
                    Expr::build_number_dec(spanned_test!(18, "1", 19), None)
                ))
            );
        }

        #[test]
        fn unary_operators_parse_as_expected() {
            assert_eq!(
                parse_expr("!~-&*x", "<test>"),
                Ok(Expr::build_not(
                    Span::from_positions_and_file(0, 6, "<test>"),
                    Expr::build_bit_not(
                        Span::from_positions_and_file(1, 6, "<test>"),
                        Expr::build_neg(
                            Span::from_positions_and_file(2, 6, "<test>"),
                            Expr::build_address_of(
                                Span::from_positions_and_file(3, 6, "<test>"),
                                Expr::build_deref(
                                    Span::from_positions_and_file(4, 6, "<test>"),
                                    Expr::build_ident(spanned_test!(5, "x", 6))
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
                parse_expr("x[x].x->x", "<test>"),
                Ok(Expr::build_arrow(
                    Expr::build_dot(
                        Expr::build_index(
                            Span::from_positions_and_file(0, 4, "<test>"),
                            Expr::build_ident(spanned_test!(0, "x", 1)),
                            Expr::build_ident(spanned_test!(2, "x", 3))
                        ),
                        spanned_test!(5, "x", 6)
                    ),
                    spanned_test!(8, "x", 9)
                ))
            );
        }

        #[test]
        fn function_calls_parse_as_expected() {
            assert_eq!(
                parse_expr("f(x, y)", "<test>"),
                Ok(Expr::build_call(
                    Span::from_positions_and_file(0, 7, "<test>"),
                    Expr::build_ident(spanned_test!(0, "f", 1)),
                    spanned_test!(
                        1,
                        vec![
                            Expr::build_ident(spanned_test!(2, "x", 3)),
                            Expr::build_ident(spanned_test!(5, "y", 6))
                        ],
                        7
                    )
                ))
            );
        }

        #[test]
        fn ternary_operator_parses_as_expected() {
            assert_eq!(
                parse_expr("a ? b : c", "<test>"),
                Ok(Expr::build_ternary(
                    Expr::build_ident(spanned_test!(0, "a", 1)),
                    Expr::build_ident(spanned_test!(4, "b", 5)),
                    Expr::build_ident(spanned_test!(8, "c", 9))
                ))
            );
        }

        #[test]
        fn casts_parse_as_expected() {
            assert_eq!(
                parse_expr("x as T", "<test>"),
                Ok(Expr::build_cast(
                    Expr::build_ident(spanned_test!(0, "x", 1)),
                    Type::build_ident(spanned_test!(5, "T", 6))
                ))
            );
        }

        mod literals {
            use super::*;
            use crate::lexer::{StringTok, ZrcString};

            #[test]
            fn number_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("1", "<test>"),
                    Ok(Expr::build_number_dec(spanned_test!(0, "1", 1), None))
                );
            }

            #[test]
            fn string_literals_parse_as_expected() {
                assert_eq!(
                    parse_expr("\"x\"", "<test>"),
                    Ok(Expr::build_string(spanned_test!(
                        0,
                        ZrcString(vec![StringTok::Text("x")]),
                        3
                    )))
                );
            }

            #[test]
            fn identifiers_parse_as_expected() {
                assert_eq!(
                    parse_expr("x", "<test>"),
                    Ok(Expr::build_ident(spanned_test!(0, "x", 1)))
                );
            }

            #[test]
            fn booleans_parse_as_expected() {
                assert_eq!(
                    parse_expr("true", "<test>"),
                    Ok(Expr::build_bool(spanned_test!(0, true, 4)))
                );
            }

            #[test]
            fn struct_construction_with_named_type_parses() {
                // Test: new Point { x: 1, y: 2 }
                let result = parse_expr("new Point { x: 1, y: 2 }", "<test>");
                assert!(
                    result.is_ok(),
                    "Failed to parse struct construction: {result:?}"
                );

                let expr = result.expect("Should have parsed successfully");
                // Verify it's a struct construction by checking the display output
                let output = format!("{expr}");
                assert!(
                    output.contains("new Point"),
                    "Expected 'new Point' in output, got: {output}"
                );
            }

            #[test]
            fn struct_construction_with_empty_fields_parses() {
                // Test: new EmptyStruct { }
                let result = parse_expr("new EmptyStruct { }", "<test>");
                assert!(
                    result.is_ok(),
                    "Failed to parse empty struct construction: {result:?}"
                );
            }

            #[test]
            fn struct_construction_with_anonymous_type_parses() {
                // Test: new struct { x: i32 } { x: 42 }
                let result = parse_expr("new struct { x: i32 } { x: 42 }", "<test>");
                assert!(
                    result.is_ok(),
                    "Failed to parse anonymous struct construction: {result:?}"
                );

                let expr = result.expect("Should have parsed successfully");
                let output = format!("{expr}");
                assert!(
                    output.contains("new struct"),
                    "Expected 'new struct' in output, got: {output}"
                );
            }

            #[test]
            fn struct_construction_with_multiple_fields_parses() {
                // Test: new Color { r: 255, g: 128, b: 64 }
                let result = parse_expr("new Color { r: 255, g: 128, b: 64 }", "<test>");
                assert!(
                    result.is_ok(),
                    "Failed to parse multi-field struct construction: {result:?}"
                );
            }

            #[test]
            fn struct_construction_with_expression_values_parses() {
                // Test: new Point { x: 1 + 2, y: 3 * 4 }
                let result = parse_expr("new Point { x: 1 + 2, y: 3 * 4 }", "<test>");
                assert!(
                    result.is_ok(),
                    "Failed to parse struct construction with expressions: {result:?}"
                );
            }
        }
    }

    mod ty {}

    mod stmt_list {}

    mod program {}
}
