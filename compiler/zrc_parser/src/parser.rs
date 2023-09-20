//! Functions to parse Zirco into an Abstract Syntax Tree

use lalrpop_util::{ErrorRecovery, ParseError};

use super::{
    ast::{
        expr::Expr,
        stmt::{Declaration, Stmt},
    },
    lexer,
};
use crate::internal_parser;

/// An error returned from one of the Zirco parsing functions, like [`parse_program`].
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
pub fn parse_program(input: &str) -> Result<Vec<Declaration>, ZircoParserError<Vec<Declaration>>> {
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

#[cfg(test)]
mod tests {
    use super::{super::ast::ty::Type, *};
    use crate::box_arguments;

    #[test]
    fn basic_expr_works_as_expected() {
        assert_eq!(
            parse_expr("1 + 2 * 3"),
            Ok(box_arguments!(
                Expr::Addition,
                Expr::NumberLiteral("1".to_string()),
                box_arguments!(
                    Expr::Multiplication,
                    Expr::NumberLiteral("2".to_string()),
                    Expr::NumberLiteral("3".to_string())
                )
            ))
        );
    }

    #[test]
    fn unexpected_eof_errors() {
        let result = parse_expr("1+").unwrap_err();

        if let ZircoParserError::Recoverable { errors, partial } = result {
            assert_eq!(
                partial,
                box_arguments!(
                    Expr::Addition,
                    Expr::NumberLiteral("1".to_string()),
                    Expr::Error
                )
            );
            assert_eq!(errors.len(), 1);
            if let ParseError::UnrecognizedEof { location, .. } = &errors[0].error {
                assert_eq!(location, &2);
            } else {
                panic!("Unexpected error type");
            }
            assert_eq!(errors[0].dropped_tokens, vec![]);
        } else {
            panic!("Expected recoverable error");
        }
    }

    #[test]
    fn dangling_else_binds_correctly() {
        assert_eq!(
            parse_stmt("if (a) if (b) c; else d;"),
            Ok(Stmt::IfStmt(
                Expr::Identifier("a".to_string()),
                Box::new(Stmt::IfElseStmt(
                    Expr::Identifier("b".to_string()),
                    Box::new(Stmt::ExprStmt(Expr::Identifier("c".to_string()))),
                    Box::new(Stmt::ExprStmt(Expr::Identifier("d".to_string())))
                ))
            ))
        )
    }

    #[test]
    fn simple_declaration_parses() {
        assert_eq!(
            parse_stmt("let a = 1;"),
            Ok(Stmt::Declaration(Declaration::DeclarationList(vec![
                LetDeclaration {
                    name: ("a".to_string()),
                    ty: None,
                    value: Some(Expr::NumberLiteral("1".to_string()).into())
                }
            ])))
        )
    }

    #[test]
    fn small_function_declaration_parses() {
        assert_eq!(
            parse_stmt("fn add(a: i32, b: i32) -> i32 { return a + b; }"),
            Ok(Stmt::Declaration(Declaration::FunctionDefinition {
                name: ("add".to_string()),
                parameters: vec![
                    ArgumentDeclaration {
                        name: ("a".to_string()),
                        ty: Type::Identifier("i32".to_string())
                    },
                    ArgumentDeclaration {
                        name: ("b".to_string()),
                        ty: Type::Identifier("i32".to_string())
                    }
                ],
                return_type: Some(Type::Identifier("i32".to_string())),
                body: vec![Stmt::ReturnStmt(Some(Expr::Addition(
                    Box::new(Expr::Identifier("a".to_string())),
                    Box::new(Expr::Identifier("b".to_string()))
                )))]
            }))
        )
    }

    #[test]
    fn larger_program_parses() {
        assert_eq!(
            parse_program(concat!(
                "fn add(a: i32, b: i32) -> i32 {\n",
                "    return a + b;\n",
                "}\n",
                "\n",
                "fn subtract(a: i32, b: i32) -> i32 {\n",
                "    return a - b;\n",
                "}\n",
                "\n",
                "fn main() {\n",
                "    let a = 1, b: i32 = 2;\n",
                "    let c = add(a, b);\n",
                "    let d = subtract(a, b);\n",
                "    return c + d;\n",
                "}",
            )),
            Ok(vec![
                Declaration::FunctionDefinition {
                    name: ("add".to_string()),
                    parameters: vec![
                        ArgumentDeclaration {
                            name: ("a".to_string()),
                            ty: Type::Identifier("i32".to_string())
                        },
                        ArgumentDeclaration {
                            name: ("b".to_string()),
                            ty: Type::Identifier("i32".to_string())
                        }
                    ],
                    return_type: Some(Type::Identifier("i32".to_string())),
                    body: vec![Stmt::ReturnStmt(Some(Expr::Addition(
                        Box::new(Expr::Identifier("a".to_string())),
                        Box::new(Expr::Identifier("b".to_string()))
                    )))]
                },
                Declaration::FunctionDefinition {
                    name: ("subtract".to_string()),
                    parameters: vec![
                        ArgumentDeclaration {
                            name: ("a".to_string()),
                            ty: Type::Identifier("i32".to_string())
                        },
                        ArgumentDeclaration {
                            name: ("b".to_string()),
                            ty: Type::Identifier("i32".to_string())
                        }
                    ],
                    return_type: Some(Type::Identifier("i32".to_string())),
                    body: vec![Stmt::ReturnStmt(Some(Expr::Subtraction(
                        Box::new(Expr::Identifier("a".to_string())),
                        Box::new(Expr::Identifier("b".to_string()))
                    )))]
                },
                Declaration::FunctionDefinition {
                    name: ("main".to_string()),
                    parameters: vec![],
                    return_type: None,
                    body: vec![
                        Stmt::Declaration(Declaration::DeclarationList(vec![
                            LetDeclaration {
                                name: ("a".to_string()),
                                ty: None,
                                value: Some(Expr::NumberLiteral("1".to_string()).into())
                            },
                            LetDeclaration {
                                name: ("b".to_string()),
                                ty: Some(Type::Identifier("i32".to_string())),
                                value: Some(Expr::NumberLiteral("2".to_string()).into())
                            },
                        ])),
                        Stmt::Declaration(Declaration::DeclarationList(vec![LetDeclaration {
                            name: ("c".to_string()),
                            ty: None,
                            value: Some(Expr::Call(
                                Box::new(Expr::Identifier("add".to_string())),
                                vec![
                                    Expr::Identifier("a".to_string()),
                                    Expr::Identifier("b".to_string())
                                ]
                            ))
                        }])),
                        Stmt::Declaration(Declaration::DeclarationList(vec![LetDeclaration {
                            name: ("d".to_string()),
                            ty: None,
                            value: Some(Expr::Call(
                                Box::new(Expr::Identifier("subtract".to_string())),
                                vec![
                                    Expr::Identifier("a".to_string()),
                                    Expr::Identifier("b".to_string())
                                ]
                            ))
                        }])),
                        Stmt::ReturnStmt(Some(Expr::Addition(
                            Box::new(Expr::Identifier("c".to_string())),
                            Box::new(Expr::Identifier("d".to_string()))
                        )))
                    ]
                }
            ])
        )
    }
}
