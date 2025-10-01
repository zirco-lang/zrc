//! Abstract Syntax Tree representation for the Zirco programming language
//!
//! This module contains the AST representation for the Zirco programming
//! language, emitted by the parser.

use std::fmt::Display;

use zrc_utils::span::Spanned;

pub mod expr;
pub mod stmt;
pub mod ty;

/// A complete Zirco program (a collection of top-level declarations)
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'input>(pub Vec<Spanned<stmt::Declaration<'input>>>);

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
