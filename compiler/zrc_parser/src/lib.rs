//! Parser for the Zirco programming language
//!
//! This crate contains the lexer, parser, and abstract syntax tree
//! representation for the Zirco programming language. It is used by the
//! compiler to parse Zirco source code into an AST.

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(
    /// Direct access to the LALRPOP parser. **Do not use this module.** Use the
    /// [`parser`] module instead.
    ///
    /// Again. **Don't use this.** The second you import this into your code,
    /// you're setting yourself up to shoot yourself in the foot. **Just use
    /// [`parser`].** There is almost *no* reason you would need this module
    /// instead, unless you need to handle the underlying errors manually,
    /// which I doubt.
    #[allow(clippy::all)]
    #[allow(clippy::nursery)]
    #[allow(clippy::pedantic)]
    #[allow(missing_docs)]
    #[allow(clippy::missing_docs_in_private_items)]
    internal_parser
);

pub mod ast;
pub mod lexer;
pub mod parser;
