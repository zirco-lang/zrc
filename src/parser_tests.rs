#![cfg(test)]

use crate::ast::Expr;
use crate::box_arguments;
use crate::lexer;
use crate::parser;

mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let input = "1 + 2 * 3";
        let result = parser::ExprParser::new().parse(lexer::Lexer::new(input));
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
    }
}
