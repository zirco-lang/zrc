#![cfg(test)]

use crate::ast::Expr;
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
                Expr::new_addition,
                crate::ast::Term::new_number_literal("1".to_string()),
                box_arguments!(
                    crate::ast::Factor::new_multiplication,
                    crate::ast::Factor::new_number_literal("2".to_string()),
                    crate::ast::Unary::new_number_literal("3".to_string())
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
                Expr::new_addition,
                crate::ast::Term::new_number_literal("1".to_string()),
                crate::ast::Factor::new_error()
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
            Ok(Expr::new_call(
                Box::new(box_arguments!(
                    crate::ast::Postfix::new_namespace_access,
                    crate::ast::Postfix::new_identifier("io".to_string()),
                    crate::ast::IDENTIFIER::new_identifier("println".to_string())
                )),
                vec![crate::ast::Assignment::StringLiteral("\"hi\"".to_string())]
            ))
        );
        assert_eq!(errors, vec![]);
    }
}
