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
    use super::{
        super::ast::{expr::*, stmt::*, ty::*},
        *,
    };

    #[test]
    fn basic_expr_works_as_expected() {
        assert_eq!(
            parse_expr("1 + 2 * 3"),
            Ok(Expr::Arithmetic(
                Arithmetic::Addition,
                Box::new(Expr::NumberLiteral("1".to_string())),
                Box::new(Expr::Arithmetic(
                    Arithmetic::Multiplication,
                    Box::new(Expr::NumberLiteral("2".to_string())),
                    Box::new(Expr::NumberLiteral("3".to_string()))
                ))
            )),
        );
    }

    #[test]
    fn unexpected_eof_errors() {
        let result = parse_expr("1+").unwrap_err();

        if let ZircoParserError::Recoverable { errors, partial } = result {
            assert_eq!(
                partial,
                Expr::Arithmetic(
                    Arithmetic::Addition,
                    Box::new(Expr::NumberLiteral("1".to_string())),
                    Box::new(Expr::Error)
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
                Box::new(Stmt::IfStmt(
                    Expr::Identifier("b".to_string()),
                    Box::new(Stmt::ExprStmt(Expr::Identifier("c".to_string()))),
                    Some(Box::new(Stmt::ExprStmt(Expr::Identifier("d".to_string()))))
                )),
                None
            ))
        )
    }

    #[test]
    fn simple_declaration_parses() {
        assert_eq!(
            parse_stmt("let a = 1;"),
            Ok(Stmt::DeclarationList(vec![LetDeclaration {
                name: ("a".to_string()),
                ty: None,
                value: Some(Expr::NumberLiteral("1".to_string()).into())
            }]))
        )
    }

    #[test]
    fn small_function_declaration_parses() {
        assert_eq!(
            parse_program("fn add(a: i32, b: i32) -> i32 { return a + b; }"),
            Ok(vec![Declaration::FunctionDeclaration {
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
                body: Some(vec![Stmt::ReturnStmt(Some(Expr::Arithmetic(
                    Arithmetic::Addition,
                    Box::new(Expr::Identifier("a".to_string())),
                    Box::new(Expr::Identifier("b".to_string()))
                )))])
            }])
        )
    }

    #[test]
    fn for_loop_test() {
        assert_eq!(
            parse_program("fn main() { for (let i = 0;;) {} }"),
            Ok(vec![Declaration::FunctionDeclaration {
                name: "main".to_string(),
                parameters: vec![],
                return_type: None,
                body: Some(vec![Stmt::ForStmt {
                    init: Some(Box::new(vec![LetDeclaration {
                        name: "i".to_string(),
                        ty: None,
                        value: Some(Expr::NumberLiteral("0".to_string()))
                    }])),
                    cond: None,
                    post: None,
                    body: Box::new(Stmt::BlockStmt(vec![]))
                }])
            }])
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
                Declaration::FunctionDeclaration {
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
                    body: Some(vec![Stmt::ReturnStmt(Some(Expr::Arithmetic(
                        Arithmetic::Addition,
                        Box::new(Expr::Identifier("a".to_string())),
                        Box::new(Expr::Identifier("b".to_string()))
                    )))])
                },
                Declaration::FunctionDeclaration {
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
                    body: Some(vec![Stmt::ReturnStmt(Some(Expr::Arithmetic(
                        Arithmetic::Subtraction,
                        Box::new(Expr::Identifier("a".to_string())),
                        Box::new(Expr::Identifier("b".to_string()))
                    )))])
                },
                Declaration::FunctionDeclaration {
                    name: ("main".to_string()),
                    parameters: vec![],
                    return_type: None,
                    body: Some(vec![
                        Stmt::DeclarationList(vec![
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
                        ]),
                        Stmt::DeclarationList(vec![LetDeclaration {
                            name: ("c".to_string()),
                            ty: None,
                            value: Some(Expr::Call(
                                Box::new(Expr::Identifier("add".to_string())),
                                vec![
                                    Expr::Identifier("a".to_string()),
                                    Expr::Identifier("b".to_string())
                                ]
                            ))
                        }]),
                        Stmt::DeclarationList(vec![LetDeclaration {
                            name: ("d".to_string()),
                            ty: None,
                            value: Some(Expr::Call(
                                Box::new(Expr::Identifier("subtract".to_string())),
                                vec![
                                    Expr::Identifier("a".to_string()),
                                    Expr::Identifier("b".to_string())
                                ]
                            ))
                        }]),
                        Stmt::ReturnStmt(Some(Expr::Arithmetic(
                            Arithmetic::Addition,
                            Box::new(Expr::Identifier("c".to_string())),
                            Box::new(Expr::Identifier("d".to_string()))
                        )))
                    ])
                }
            ])
        )
    }
}
