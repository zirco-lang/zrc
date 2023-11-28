//! Type-checked AST for the Zirco programming language
//!
//! This is similar to the [standard AST](zrc_parser::ast), but with type
//! information attached to it.
//!
//! Note that we only attach source locations to code that would exist after code generation,
//! meaning inlined types do not get a span.

pub mod expr;
pub mod stmt;
pub mod ty;
