pub mod ast;
pub mod lexer;
mod parser_tests;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub parser);

/// Call the function $f with the passed arguments wrapped in Box-es
#[macro_export]
macro_rules! box_arguments {
    ($f:expr,$($a:expr),+) => ( $f($(Box::new($a)),+) );
}

fn main() {}
