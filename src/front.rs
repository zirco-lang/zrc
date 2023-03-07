pub mod ast;
pub mod lexer;
pub mod parser;

#[macro_export]
/// Parse a string as an expression with the Zirco parser.
/// Returns a `Result` with the parsed Expr or a `ZircoParserError<Expr>`.
macro_rules! parse_expr {
    ($input: expr) => {
        let mut errors = Vec::new();
        let result = parser::ExprParser::new().parse(&mut errors, lexer::ZircoLexer::new($input));
        match result {
            Err(e) => Err(ZircoParserError::Fatal(e)),
            Ok(expr) => {
                if errors.is_empty() {
                    Ok(expr)
                } else {
                    Err(ZircoParserError::Recoverable {
                        errors,
                        partial: expr,
                    })
                }
            }
        }
    };
}
