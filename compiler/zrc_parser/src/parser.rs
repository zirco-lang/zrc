//! Functions to parse Zirco into an Abstract Syntax Tree

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
            Ok(Expr(Spanned(
                0,
                ExprKind::Arithmetic(
                    Arithmetic::Addition,
                    Box::new(Expr(Spanned(
                        0,
                        ExprKind::NumberLiteral("1".to_string()),
                        1
                    ))),
                    Box::new(Expr(Spanned(
                        4,
                        ExprKind::Arithmetic(
                            Arithmetic::Multiplication,
                            Box::new(Expr(Spanned(
                                4,
                                ExprKind::NumberLiteral("2".to_string()),
                                5
                            ))),
                            Box::new(Expr(Spanned(
                                8,
                                ExprKind::NumberLiteral("3".to_string()),
                                9
                            )))
                        ),
                        9
                    )))
                ),
                9
            ))),
        );
    }

    #[test]
    fn unexpected_eof_errors() {
        let result = parse_expr("1+").unwrap_err();

        if let ZircoParserError::Recoverable { errors, partial } = result {
            assert_eq!(
                partial,
                Expr(Spanned(
                    0,
                    ExprKind::Arithmetic(
                        Arithmetic::Addition,
                        Box::new(Expr(Spanned(
                            0,
                            ExprKind::NumberLiteral("1".to_string()),
                            1
                        ))),
                        Box::new(Expr(Spanned(2, ExprKind::Error, 2)))
                    ),
                    2
                ))
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
            Ok(Stmt(Spanned(
                0,
                StmtKind::IfStmt(
                    Expr(Spanned(4, ExprKind::Identifier("a".to_string()), 5)),
                    Box::new(Stmt(Spanned(
                        7,
                        StmtKind::IfStmt(
                            Expr(Spanned(11, ExprKind::Identifier("b".to_string()), 12)),
                            Box::new(Stmt(Spanned(
                                14,
                                StmtKind::ExprStmt(Expr(Spanned(
                                    14,
                                    ExprKind::Identifier("c".to_string()),
                                    15
                                ))),
                                16
                            ))),
                            Some(Box::new(Stmt(Spanned(
                                22,
                                StmtKind::ExprStmt(Expr(Spanned(
                                    22,
                                    ExprKind::Identifier("d".to_string()),
                                    23
                                ))),
                                24
                            ))))
                        ),
                        24
                    ))),
                    None
                ),
                24
            )))
        );
    }

    #[test]
    fn simple_declaration_parses() {
        assert_eq!(
            parse_stmt("let a = 1;"),
            Ok(Stmt(Spanned(
                0,
                StmtKind::Declaration(Declaration::DeclarationList(vec![Spanned(
                    4,
                    LetDeclaration {
                        name: Spanned(4, "a".to_string(), 5),
                        ty: None,
                        value: Some(Expr(Spanned(
                            8,
                            ExprKind::NumberLiteral("1".to_string()),
                            9
                        )))
                    },
                    9
                )])),
                10
            )))
        );
    }

    #[test]
    fn small_function_declaration_parses() {
        assert_eq!(
            parse_stmt("fn add(a: i32, b: i32) -> i32 { return a + b; }"),
            Ok(Stmt(Spanned(
                0,
                StmtKind::Declaration(Declaration::FunctionDefinition {
                    name: Spanned(3, "add".to_string(), 6),
                    parameters: Spanned(
                        7,
                        vec![
                            Spanned(
                                7,
                                ArgumentDeclaration {
                                    name: Spanned(7, "a".to_string(), 8),
                                    ty: Some(Type(Spanned(
                                        10,
                                        TypeKind::Identifier("i32".to_string()),
                                        13
                                    )))
                                },
                                13
                            ),
                            Spanned(
                                15,
                                ArgumentDeclaration {
                                    name: Spanned(15, "b".to_string(), 16),
                                    ty: Some(Type(Spanned(
                                        18,
                                        TypeKind::Identifier("i32".to_string()),
                                        21
                                    )))
                                },
                                21
                            )
                        ],
                        21
                    ),
                    return_type: Some(Type(Spanned(
                        26,
                        TypeKind::Identifier("i32".to_string()),
                        29
                    ))),
                    body: Spanned(
                        32,
                        vec![Stmt(Spanned(
                            32,
                            StmtKind::ReturnStmt(Some(Expr(Spanned(
                                39,
                                ExprKind::Arithmetic(
                                    Arithmetic::Addition,
                                    Box::new(Expr(Spanned(
                                        39,
                                        ExprKind::Identifier("a".to_string()),
                                        40
                                    ))),
                                    Box::new(Expr(Spanned(
                                        43,
                                        ExprKind::Identifier("b".to_string()),
                                        44
                                    )))
                                ),
                                44
                            )))),
                            45
                        ))],
                        45
                    )
                }),
                47
            )))
        );
    }

    #[test]
    fn for_loop_test() {
        assert_eq!(
            parse_program("fn main() { for (let i = 0;;) {} }"),
            Ok(vec![Spanned(
                0,
                Declaration::FunctionDefinition {
                    name: Spanned(3, "main".to_string(), 7),
                    parameters: Spanned(8, vec![], 8),
                    return_type: None,
                    body: Spanned(
                        12,
                        vec![Stmt(Spanned(
                            12,
                            StmtKind::ForStmt {
                                init: Some(Box::new(Declaration::DeclarationList(vec![Spanned(
                                    21,
                                    LetDeclaration {
                                        name: Spanned(21, "i".to_string(), 22),
                                        ty: None,
                                        value: Some(Expr(Spanned(
                                            25,
                                            ExprKind::NumberLiteral("0".to_string()),
                                            26
                                        )))
                                    },
                                    26
                                )]))),
                                cond: None,
                                post: None,
                                body: Box::new(Stmt(Spanned(30, StmtKind::BlockStmt(vec![]), 32)))
                            },
                            32
                        ))],
                        32
                    )
                },
                34
            )])
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
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
                Spanned(
                    0,
                    Declaration::FunctionDefinition {
                        name: Spanned(3, "add".to_string(), 6),
                        parameters: Spanned(
                            7,
                            vec![
                                Spanned(
                                    7,
                                    ArgumentDeclaration {
                                        name: Spanned(7, "a".to_string(), 8),
                                        ty: Some(Type(Spanned(
                                            10,
                                            TypeKind::Identifier("i32".to_string()),
                                            13
                                        )))
                                    },
                                    13
                                ),
                                Spanned(
                                    15,
                                    ArgumentDeclaration {
                                        name: Spanned(15, "b".to_string(), 16),
                                        ty: Some(Type(Spanned(
                                            18,
                                            TypeKind::Identifier("i32".to_string()),
                                            21
                                        )))
                                    },
                                    21
                                )
                            ],
                            21
                        ),
                        return_type: Some(Type(Spanned(
                            26,
                            TypeKind::Identifier("i32".to_string()),
                            29
                        ))),
                        body: Spanned(
                            36,
                            vec![Stmt(Spanned(
                                36,
                                StmtKind::ReturnStmt(Some(Expr(Spanned(
                                    43,
                                    ExprKind::Arithmetic(
                                        Arithmetic::Addition,
                                        Box::new(Expr(Spanned(
                                            43,
                                            ExprKind::Identifier("a".to_string()),
                                            44
                                        ))),
                                        Box::new(Expr(Spanned(
                                            47,
                                            ExprKind::Identifier("b".to_string()),
                                            48
                                        )))
                                    ),
                                    48
                                )))),
                                49
                            ))],
                            49
                        )
                    },
                    51
                ),
                Spanned(
                    53,
                    Declaration::FunctionDefinition {
                        name: Spanned(56, "subtract".to_string(), 64),
                        parameters: Spanned(
                            65,
                            vec![
                                Spanned(
                                    65,
                                    ArgumentDeclaration {
                                        name: Spanned(65, "a".to_string(), 66),
                                        ty: Some(Type(Spanned(
                                            68,
                                            TypeKind::Identifier("i32".to_string()),
                                            71
                                        )))
                                    },
                                    71
                                ),
                                Spanned(
                                    73,
                                    ArgumentDeclaration {
                                        name: Spanned(73, "b".to_string(), 74),
                                        ty: Some(Type(Spanned(
                                            76,
                                            TypeKind::Identifier("i32".to_string()),
                                            79
                                        )))
                                    },
                                    79
                                )
                            ],
                            79
                        ),
                        return_type: Some(Type(Spanned(
                            84,
                            TypeKind::Identifier("i32".to_string()),
                            87
                        ))),
                        body: Spanned(
                            94,
                            vec![Stmt(Spanned(
                                94,
                                StmtKind::ReturnStmt(Some(Expr(Spanned(
                                    101,
                                    ExprKind::Arithmetic(
                                        Arithmetic::Subtraction,
                                        Box::new(Expr(Spanned(
                                            101,
                                            ExprKind::Identifier("a".to_string()),
                                            102
                                        ))),
                                        Box::new(Expr(Spanned(
                                            105,
                                            ExprKind::Identifier("b".to_string()),
                                            106
                                        )))
                                    ),
                                    106
                                )))),
                                107
                            ))],
                            107
                        )
                    },
                    109
                ),
                Spanned(
                    111,
                    Declaration::FunctionDefinition {
                        name: Spanned(114, "main".to_string(), 118),
                        parameters: Spanned(119, vec![], 119),
                        return_type: None,
                        body: Spanned(
                            127,
                            vec![
                                Stmt(Spanned(
                                    127,
                                    StmtKind::Declaration(Declaration::DeclarationList(vec![
                                        Spanned(
                                            131,
                                            LetDeclaration {
                                                name: Spanned(131, "a".to_string(), 132),
                                                ty: None,
                                                value: Some(Expr(Spanned(
                                                    135,
                                                    ExprKind::NumberLiteral("1".to_string()).into(),
                                                    136
                                                )))
                                            },
                                            136
                                        ),
                                        Spanned(
                                            138,
                                            LetDeclaration {
                                                name: Spanned(138, "b".to_string(), 139),
                                                ty: Some(Type(Spanned(
                                                    141,
                                                    TypeKind::Identifier("i32".to_string()),
                                                    144
                                                ))),
                                                value: Some(Expr(Spanned(
                                                    147,
                                                    ExprKind::NumberLiteral("2".to_string()).into(),
                                                    148
                                                )))
                                            },
                                            148
                                        ),
                                    ])),
                                    149
                                )),
                                Stmt(Spanned(
                                    154,
                                    StmtKind::Declaration(Declaration::DeclarationList(vec![
                                        Spanned(
                                            158,
                                            LetDeclaration {
                                                name: Spanned(158, "c".to_string(), 159),
                                                ty: None,
                                                value: Some(Expr(Spanned(
                                                    162,
                                                    ExprKind::Call(
                                                        Box::new(Expr(Spanned(
                                                            162,
                                                            ExprKind::Identifier("add".to_string()),
                                                            165
                                                        ))),
                                                        Spanned(
                                                            166,
                                                            vec![
                                                                Expr(Spanned(
                                                                    166,
                                                                    ExprKind::Identifier(
                                                                        "a".to_string()
                                                                    ),
                                                                    167
                                                                )),
                                                                Expr(Spanned(
                                                                    169,
                                                                    ExprKind::Identifier(
                                                                        "b".to_string()
                                                                    ),
                                                                    170
                                                                ))
                                                            ],
                                                            170
                                                        )
                                                    ),
                                                    171
                                                )))
                                            },
                                            171
                                        )
                                    ])),
                                    172
                                )),
                                Stmt(Spanned(
                                    177,
                                    StmtKind::Declaration(Declaration::DeclarationList(vec![
                                        Spanned(
                                            181,
                                            LetDeclaration {
                                                name: Spanned(181, "d".to_string(), 182),
                                                ty: None,
                                                value: Some(Expr(Spanned(
                                                    185,
                                                    ExprKind::Call(
                                                        Box::new(Expr(Spanned(
                                                            185,
                                                            ExprKind::Identifier(
                                                                "subtract".to_string()
                                                            ),
                                                            193
                                                        ))),
                                                        Spanned(
                                                            194,
                                                            vec![
                                                                Expr(Spanned(
                                                                    194,
                                                                    ExprKind::Identifier(
                                                                        "a".to_string()
                                                                    ),
                                                                    195
                                                                )),
                                                                Expr(Spanned(
                                                                    197,
                                                                    ExprKind::Identifier(
                                                                        "b".to_string()
                                                                    ),
                                                                    198
                                                                ))
                                                            ],
                                                            198
                                                        )
                                                    ),
                                                    199
                                                )))
                                            },
                                            199
                                        )
                                    ])),
                                    200
                                )),
                                Stmt(Spanned(
                                    205,
                                    StmtKind::ReturnStmt(Some(Expr(Spanned(
                                        212,
                                        ExprKind::Arithmetic(
                                            Arithmetic::Addition,
                                            Box::new(Expr(Spanned(
                                                212,
                                                ExprKind::Identifier("c".to_string()),
                                                213
                                            ))),
                                            Box::new(Expr(Spanned(
                                                216,
                                                ExprKind::Identifier("d".to_string()),
                                                217
                                            )))
                                        ),
                                        217
                                    )))),
                                    218
                                ))
                            ],
                            218
                        )
                    },
                    220
                )
            ])
        );
    }
}
