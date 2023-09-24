//! Type representation for the Zirco [TAST](super)

use std::{collections::HashMap, fmt::Display};

use super::super::BlockReturnType;

/// The possible Zirco types
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    /// `i8`
    I8,
    /// `u8`
    U8,
    /// `i16`
    I16,
    /// `u16`
    U16,
    /// `i32`
    I32,
    /// `u32`
    U32,
    /// `i64`
    I64,
    /// `u64`
    U64,
    /// `isize`
    Isize,
    /// `usize`
    Usize,
    /// `void`, only producible by calling a void function (`fn()`)
    Void,
    /// `bool`
    Bool, /* TODO: need an "any Int" type that implicitly casts to all int types but becomes
           * i32 when assigned to a value */
    /// `*T`
    Ptr(Box<Type>),
    /// `fn(A, B) -> T`
    Fn(Vec<Type>, Box<BlockReturnType>),
    /// Struct type literals
    Struct(HashMap<String, Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::I8 => write!(f, "i8"),
            Self::U8 => write!(f, "u8"),
            Self::I16 => write!(f, "i16"),
            Self::U16 => write!(f, "u16"),
            Self::I32 => write!(f, "i32"),
            Self::U32 => write!(f, "u32"),
            Self::I64 => write!(f, "i64"),
            Self::U64 => write!(f, "u64"),
            Self::Isize => write!(f, "isize"),
            Self::Usize => write!(f, "usize"),
            Self::Bool => write!(f, "bool"),
            Self::Ptr(t) => write!(f, "*({t})"),
            Self::Void => write!(f, "void"),
            Self::Fn(args, brt) => write!(
                f,
                "(fn({}){})",
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                match *brt {
                    BlockReturnType::Return(ret) => format!(" -> {ret}"),
                    BlockReturnType::Void => String::new(),
                }
            ),
            Self::Struct(fields) => write!(
                f,
                "(struct {{ {} }})",
                fields
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    /// Returns `true` if this is an integer type like [`Type::I8`].
    #[must_use]
    pub const fn is_integer(&self) -> bool {
        use Type::{Isize, Usize, I16, I32, I64, I8, U16, U32, U64, U8};
        matches!(
            self,
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Isize | Usize
        )
    }

    /// Returns `true` if this is a signed integer type like [`Type::I8`].
    #[must_use]
    pub const fn is_signed_integer(&self) -> bool {
        use Type::{Isize, I16, I32, I64, I8};
        matches!(self, I8 | I16 | I32 | I64 | Isize)
    }
}
