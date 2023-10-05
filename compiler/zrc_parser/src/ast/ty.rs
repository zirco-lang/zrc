//! Type representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Type`] struct. It
//! contains all the different type kinds in Zirco.

use std::fmt::Display;

/// A valid Zirco AST type
#[derive(PartialEq, Debug, Clone)]
pub struct Type(pub super::Spanned<TypeKind>);

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeKind {
    /// An identifier, such as `i32`
    Identifier(String),
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(i) => write!(f, "{i}"),
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0 .1.fmt(f)
    }
}
