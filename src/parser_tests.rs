#![cfg(test)]

use crate::ast::*;
use crate::box_arguments;
use crate::lexer;
use crate::parser;
use lalrpop_util::ParseError;

mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let mut errors = Vec::new();
        let input = "1 + 2 * 3";
        let result = parser::ExprParser::new().parse(&mut errors, lexer::ZircoLexer::new(input));
        assert_eq!(
            result,
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
        assert_eq!(errors, vec![])
    }

    #[test]
    fn test_error() {
        let mut errors = Vec::new();
        let input = "1+";
        let result = parser::ExprParser::new().parse(&mut errors, lexer::ZircoLexer::new(input));
        assert_eq!(
            result,
            Ok(box_arguments!(
                Expr::Addition,
                Term::NumberLiteral("1".to_string()),
                Factor::Error
            ))
        );

        assert_eq!(errors.len(), 1);
        if let ParseError::UnrecognizedEOF { location, .. } = &errors[0].error {
            assert_eq!(location, &2);
        } else {
            panic!("Unexpected error type");
        }
        assert_eq!(errors[0].dropped_tokens, vec![]);
    }

    #[test]
    fn namespace_test() {
        let mut errors = Vec::new();
        let input = "io::println(\"hi\")";
        let result = parser::ExprParser::new().parse(&mut errors, lexer::ZircoLexer::new(input));
        assert_eq!(
            result,
            Ok(Expr::Call(
                Box::new(box_arguments!(
                    Postfix::NamespaceAccess,
                    Postfix::Identifier("io".to_string()),
                    IDENTIFIER::Identifier("println".to_string())
                )),
                vec![Assignment::StringLiteral("\"hi\"".to_string())]
            ))
        );
        assert_eq!(errors, vec![]);
    }

    #[test]
    fn ambiguous_test() {
        let mut errors = Vec::new();
        let input = "if (a) if (b) c; else d;";
        let result = parser::StmtParser::new().parse(&mut errors, lexer::ZircoLexer::new(input));
        assert_eq!(
            result,
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
}
