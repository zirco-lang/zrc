//! Type-checked AST for the Zirco programming language
//!
//! This is similar to the [standard AST](zrc_parser::ast), but with type
//! information attached to it.

use std::fmt::Display;

use zrc_utils::span::Spanned;

pub mod expr;
pub mod stmt;
pub mod ty;

/// A complete Zirco program after type checking (a collection of top-level declarations)
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'input>(pub Vec<Spanned<stmt::TypedDeclaration<'input>>>);

impl<'input> Display for Program<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn program_display_works() {
        let input = "fn main() -> i32 {\n    return 42;\n}\nfn helper(x: i32) -> i32 {\n    return (x) + (1);\n}";

        let ast = zrc_parser::parser::parse_program(input)
            .expect("test case should have parsed correctly");

        let tast = crate::typeck::type_program(ast)
            .expect("test case should have type checked correctly");

        // The Display implementation should show the function signatures with body as "{ ... }"
        let display_output = tast.to_string();
        assert!(display_output.contains("fn main() -> i32 { ... }"));
        assert!(display_output.contains("fn helper(x: i32) -> i32 { ... }"));
    }
}

