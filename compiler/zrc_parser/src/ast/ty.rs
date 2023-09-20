//! Type representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Type`] enum. It contains all the different type kinds in Zirco.

use std::{collections::HashMap, fmt::Display};

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    /// An identifier, such as `i32`
    Identifier(String),
    Ptr(Box<Type>),
    Struct(HashMap<String, Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(i) => write!(f, "{}", i),
            Self::Ptr(t) => write!(f, "*{}", t),
            Self::Struct(members) => {
                write!(f, "struct {{ ")?;
                for (i, m) in members.iter().enumerate() {
                    write!(f, "{}: {}", m.0, m.1)?;
                    if i < members.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
