#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions)]

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(
    /// Direct access to the LALRPOP parser. Prefer to use the functions exported by front::parser instead.
    #[allow(clippy::all)]
    internal_parser
);

/// Call the function $f with the passed arguments wrapped in Box-es
#[macro_export]
macro_rules! box_arguments {
    ($f:expr,$($a:expr),+) => ( $f($(Box::new($a)),+) );
}

pub mod ast;
pub mod lexer;
pub mod parser;
