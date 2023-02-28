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
                Expr::new_number_literal("1".to_string()),
                box_arguments!(
                    Expr::new_multiplication,
                    Expr::new_number_literal("2".to_string()),
                    Expr::new_number_literal("3".to_string())
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
                Expr::new_number_literal("1".to_string()),
                Expr::Error
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
}
