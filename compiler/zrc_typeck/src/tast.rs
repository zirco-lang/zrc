//! Type-checked AST for the Zirco programming language
//!
//! This is similar to the [standard AST](zrc_parser::ast), but with type
//! information attached to it.

pub mod expr;
pub mod stmt;
pub mod ty;
