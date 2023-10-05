//! Abstract Syntax Tree representation for the Zirco programming language
//!
//! This module contains the AST representation for the Zirco programming
//! language, emitted by the parser.

/// A token with an associated span within the input.
#[derive(PartialEq, Debug, Clone)]
pub struct Spanned<T>(pub usize, pub T, pub usize);

pub mod expr;
pub mod stmt;
pub mod ty;
