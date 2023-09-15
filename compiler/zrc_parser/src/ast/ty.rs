//! Type representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Type`] enum. It contains all the different type kinds in Zirco.

use std::fmt::Display;

/// A valid Zirco AST type
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    /// An identifier, such as `i32`
    Identifier(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Identifier(i) => write!(f, "{}", i),
        }
    }
}
