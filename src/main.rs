pub mod ast;
pub mod lexer;
mod parser_tests;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub parser);

#[macro_use]
extern crate derive_new;

/// Call the function $f with the passed arguments wrapped in Box-es
#[macro_export]
macro_rules! box_arguments {
    ($f:expr,$($a:expr),+) => ( $f($(Box::new($a)),+) );
}

/// Translate into a different sub-enum of Expr
#[macro_export]
macro_rules! into_expr_type {
    ($to:tt,$val:expr) => {
        crate::ast::$to::try_from(crate::ast::Expr::from($val)).unwrap()
    };
}

fn main() {}
