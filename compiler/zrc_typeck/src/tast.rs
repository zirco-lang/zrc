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
