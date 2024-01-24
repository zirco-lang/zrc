//! Type representation for the Zirco [TAST](super)
//!
//! Note: In the TAST, we do not assign types a span, because
//! they may be obtained from an arbitrary location in the program
//! (and simply inferred). If it exists *in* the source file, it is
//! given an explicit span (e.g. argument types) however.

use std::fmt::Display;

use indexmap::IndexMap;

use super::stmt::ArgumentDeclarationList;
use crate::typeck::BlockReturnType;

/// Data attached to a [`Type::Fn`]
#[derive(Debug, Clone, PartialEq)]
pub struct Fn<'input> {
    /// The function's arguments
    pub arguments: ArgumentDeclarationList<'input>,
    /// The function's return type
    pub returns: Box<BlockReturnType<'input>>,
}
impl<'input> Display for Fn<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn({}){})",
            self.arguments,
            match &*self.returns {
                BlockReturnType::Return(ret) => format!(" -> {ret}"),
                BlockReturnType::Void => String::new(),
            }
        )
    }
}

/// The possible Zirco types
#[derive(PartialEq, Debug, Clone)]
pub enum Type<'input> {
    // WHENEVER ADDING NEW PRIMITIVES HERE, ADD THEM TO THE TYPE SCOPE IN
    // [`zrc_typeck::typeck::Scope::default`].
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
    /// `usize`
    Usize,
    /// `isize`
    Isize,
    /// `void`, only producible by calling a void function (`fn()`)
    Void,
    /// `bool`
    Bool, /* TODO: need an "any Int" type that implicitly casts to all int types but becomes
           * i32 when assigned to a value */
    /// `*T`
    Ptr(Box<Type<'input>>),
    /// `fn(A, B) -> T`
    Fn(Fn<'input>),
    /// Struct type literals. Ordered by declaration order.
    Struct(IndexMap<&'input str, Type<'input>>),
    /// Union type literals. Ordered by declaration order.
    Union(IndexMap<&'input str, Type<'input>>),
}

impl<'input> Display for Type<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(f, "i8"),
            Self::U8 => write!(f, "u8"),
            Self::I16 => write!(f, "i16"),
            Self::U16 => write!(f, "u16"),
            Self::I32 => write!(f, "i32"),
            Self::U32 => write!(f, "u32"),
            Self::I64 => write!(f, "i64"),
            Self::U64 => write!(f, "u64"),
            Self::Usize => write!(f, "usize"),
            Self::Isize => write!(f, "isize"),
            Self::Bool => write!(f, "bool"),
            Self::Ptr(pointee_ty) => write!(f, "*({pointee_ty})"),
            Self::Void => write!(f, "void"),
            Self::Fn(fn_data) => write!(f, "{fn_data}"),
            Self::Struct(fields) => write!(
                f,
                "(struct {{ {} }})",
                fields
                    .iter()
                    .map(|(key, ty)| format!("{key}: {ty}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Union(fields) => write!(
                f,
                "(union {{ {} }})",
                fields
                    .iter()
                    .map(|(key, ty)| format!("{key}: {ty}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl<'input> Type<'input> {
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

    /// Returns `true` if this is an unsigned integer type like [`Type::U8`].
    #[must_use]
    pub const fn is_unsigned_integer(&self) -> bool {
        use Type::{Usize, U16, U32, U64, U8};
        matches!(self, U8 | U16 | U32 | U64 | Usize)
    }

    /// Try to get the value we point at, or None if not a pointer.
    #[must_use]
    #[allow(clippy::wildcard_enum_match_arm)]
    pub fn into_pointee(self) -> Option<Type<'input>> {
        match self {
            Type::Ptr(x) => Some(*x),
            _ => None,
        }
    }

    /// Try to access the struct's [`IndexMap`] if we are a struct
    #[must_use]
    #[allow(clippy::wildcard_enum_match_arm)]
    pub fn into_struct_contents(self) -> Option<IndexMap<&'input str, Type<'input>>> {
        match self {
            Type::Struct(x) => Some(x),
            _ => None,
        }
    }

    /// Try to access the union's [`IndexMap`] if we are a union
    #[must_use]
    #[allow(clippy::wildcard_enum_match_arm)]
    pub fn into_union_contents(self) -> Option<IndexMap<&'input str, Type<'input>>> {
        match self {
            Type::Union(x) => Some(x),
            _ => None,
        }
    }
}
