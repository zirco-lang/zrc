use super::{ast::expr::*, ast::stmt::*, ast::ty::Type, lexer};
use crate::parser;
use lalrpop_util::{ErrorRecovery, ParseError};

#[derive(Debug, PartialEq)]
pub enum ZircoParserError<T> {
    /// An error we were able to recover from
    Recoverable {
        errors: Vec<ErrorRecovery<usize, lexer::Tok, lexer::LexicalError>>,
        partial: T,
    },
    /// An error that stopped the parser.
    Fatal(ParseError<usize, lexer::Tok, lexer::LexicalError>),
}

/// More generic macro for [parse_expr] and [parse_stmt]
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

/// Parse a string as an expression with the Zirco parser.
pub fn parse_expr(input: &str) -> Result<Expr, ZircoParserError<Expr>> {
    parse_internal!(parser::ExprParser, input)
}

/// Parse a string as a statement with the Zirco parser.
pub fn parse_stmt(input: &str) -> Result<Stmt, ZircoParserError<Stmt>> {
    parse_internal!(parser::StmtParser, input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::box_arguments;

    #[test]
    fn test_parser() {
        assert_eq!(
            parse_expr("1 + 2 * 3"),
            Ok(box_arguments!(
                Expr::Addition,
                Term::NumberLiteral("1".to_string()),
                box_arguments!(
                    Factor::Multiplication,
                    Factor::NumberLiteral("2".to_string()),
                    Unary::NumberLiteral("3".to_string())
                )
            ))
        );
    }

    #[test]
    fn test_error() {
        let result = parse_expr("1+").unwrap_err();

        if let ZircoParserError::Recoverable { errors, partial } = result {
            assert_eq!(
                partial,
                box_arguments!(
                    Expr::Addition,
                    Term::NumberLiteral("1".to_string()),
                    Factor::Error
                )
            );
            assert_eq!(errors.len(), 1);
            if let ParseError::UnrecognizedEOF { location, .. } = &errors[0].error {
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
    fn namespace_test() {
        assert_eq!(
            parse_expr("io::println(\"hi\")"),
            Ok(Expr::Call(
                Box::new(box_arguments!(
                    Postfix::NamespaceAccess,
                    Postfix::Identifier("io".to_string()),
                    IDENTIFIER::Identifier("println".to_string())
                )),
                vec![Assignment::StringLiteral("\"hi\"".to_string())]
            ))
        );
    }

    #[test]
    fn ambiguous_test() {
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
    fn let_test() {
        assert_eq!(
            parse_stmt("let a = 1;"),
            Ok(Stmt::LetDeclaration {
                name: IDENTIFIER::Identifier("a".to_string()),
                ty: None,
                value: Some(Assignment::NumberLiteral("1".to_string()).into())
            })
        )
    }

    #[test]
    fn fn_test() {
        assert_eq!(
            parse_stmt("fn add(a: int, b: int) -> int { return a + b; }"),
            Ok(Stmt::FunctionDefinition {
                name: IDENTIFIER::Identifier("add".to_string()),
                parameters: vec![
                    ArgumentDeclaration {
                        name: IDENTIFIER::Identifier("a".to_string()),
                        ty: Some(Type::Identifier(IDENTIFIER::Identifier("int".to_string())))
                    },
                    ArgumentDeclaration {
                        name: IDENTIFIER::Identifier("b".to_string()),
                        ty: Some(Type::Identifier(IDENTIFIER::Identifier("int".to_string())))
                    }
                ],
                return_type: Some(Type::Identifier(IDENTIFIER::Identifier("int".to_string()))),
                body: vec![Stmt::ReturnStmt(Some(Expr::Addition(
                    Box::new(Term::Identifier("a".to_string())),
                    Box::new(Factor::Identifier("b".to_string()))
                )))]
            })
        )
    }
}
