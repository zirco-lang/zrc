//! Type representation for the Zirco [TAST](super)
//!
//! Note: In the TAST, we do not assign types a span, because
//! they may be obtained from an arbitrary location in the program
//! (and simply inferred). If it exists *in* the source file, it is
//! given an explicit span (e.g. argument types) however.

use std::fmt::Display;

use derive_more::Display;
use indexmap::IndexMap;
use zrc_parser::lexer::NumberLiteral;

use super::stmt::ArgumentDeclarationList;

/// Data attached to a [`Type::Fn`]
#[derive(Debug, Clone, PartialEq, Display)]
#[display("(fn({arguments}) -> {returns})")]
pub struct Fn<'input> {
    /// The function's arguments
    pub arguments: ArgumentDeclarationList<'input>,
    /// The function's return type
    pub returns: Box<Type<'input>>,
}

/// Auxillary data attached to a [`Fn`] in the Global Scope
#[derive(Debug, Clone)]
pub struct FunctionDeclarationGlobalMetadata<'input> {
    /// The corresponding [`Fn`] we wrap
    pub fn_type: Fn<'input>,
    /// If a declaration exists to implement this function
    /// (Only one may exist)
    pub has_implementation: bool,
}

/// The possible Zirco types
#[derive(PartialEq, Debug, Clone)]
pub enum Type<'input> {
    // WHENEVER ADDING NEW PRIMITIVES HERE, ADD THEM TO THE TYPE SCOPE IN
    // [`zrc_typeck::typeck::scope::TypeScope::new`].
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
    /// `bool`
    Bool,
    /// `{int}` - type that represents any integer literal and implicitly
    /// coerces to any int type. Defaults to `i32` when assigned to a value
    /// without explicit type annotation.
    Int,
    /// `*T`
    Ptr(Box<Self>),
    /// `fn(A, B) -> T`
    Fn(Fn<'input>),
    /// Struct type literals. Ordered by declaration order.
    Struct(IndexMap<&'input str, Self>),
    /// Union type literals. Ordered by declaration order.
    Union(IndexMap<&'input str, Self>),
    /// Array type `[N]T`
    Array {
        /// The element type
        element_type: Box<Self>,
        /// The size integer
        size: NumberLiteral<'input>,
    },
    /// Opaque type placeholder used during type resolution for self-referential
    /// types. This is a temporary type that should be replaced with a void
    /// pointer (`*struct{}`) after the type definition is fully resolved.
    /// Opaque types should never appear in final TAST output or code
    /// generation.
    Opaque(&'input str),
}

impl Display for Type<'_> {
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
            Self::Int => write!(f, "{{int}}"),
            Self::Ptr(pointee_ty) => write!(f, "*{pointee_ty}"),
            Self::Fn(fn_data) => write!(f, "{fn_data}"),
            Self::Struct(fields) if fields.is_empty() => write!(f, "struct {{}}"),
            Self::Struct(fields) => write!(
                f,
                "struct {{ {} }}",
                fields
                    .iter()
                    .map(|(key, ty)| format!("{key}: {ty}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Union(fields) if fields.is_empty() => write!(f, "union {{}}"),
            Self::Union(fields) => write!(
                f,
                "union {{ {} }}",
                fields
                    .iter()
                    .map(|(key, ty)| format!("{key}: {ty}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Opaque(name) => write!(f, "{name}"),
            Self::Array { element_type, size } => write!(f, "[{size}]{element_type}"),
        }
    }
}

impl<'input> Type<'input> {
    /// Returns `true` if this is an integer type like [`Type::I8`].
    #[must_use]
    pub const fn is_integer(&self) -> bool {
        use Type::{I8, I16, I32, I64, Int, Isize, U8, U16, U32, U64, Usize};
        matches!(
            self,
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Isize | Usize | Int
        )
    }

    /// Returns `true` if this is a signed integer type like [`Type::I8`].
    #[must_use]
    pub const fn is_signed_integer(&self) -> bool {
        use Type::{I8, I16, I32, I64, Isize};
        matches!(self, I8 | I16 | I32 | I64 | Isize)
    }

    /// Returns `true` if this is an unsigned integer type like [`Type::U8`].
    #[must_use]
    pub const fn is_unsigned_integer(&self) -> bool {
        use Type::{U8, U16, U32, U64, Usize};
        matches!(self, U8 | U16 | U32 | U64 | Usize)
    }

    /// Try to get the value we point at, or None if not a pointer.
    #[must_use]
    #[expect(clippy::wildcard_enum_match_arm)]
    pub fn into_pointee(self) -> Option<Self> {
        match self {
            Type::Ptr(x) => Some(*x),
            _ => None,
        }
    }

    /// Try to access the struct's [`IndexMap`] if we are a struct
    #[must_use]
    #[expect(clippy::wildcard_enum_match_arm)]
    pub fn into_struct_contents(self) -> Option<IndexMap<&'input str, Self>> {
        match self {
            Type::Struct(x) => Some(x),
            _ => None,
        }
    }

    /// Try to access the union's [`IndexMap`] if we are a union
    #[must_use]
    #[expect(clippy::wildcard_enum_match_arm)]
    pub fn into_union_contents(self) -> Option<IndexMap<&'input str, Self>> {
        match self {
            Type::Union(x) => Some(x),
            _ => None,
        }
    }

    /// Get the unit type
    #[must_use]
    pub fn unit() -> Self {
        Type::Struct(IndexMap::new())
    }

    /// Check if this type can be implicitly cast to the target type.
    /// Currently supports:
    /// - `*T` -> `*struct{}` (void pointer downcast)
    /// - `{int}` -> any integer type
    ///
    /// # Examples
    ///
    /// This allows any pointer type to implicitly downcast to void pointer:
    /// ```zirco
    /// fn takes_void_ptr(ptr: *struct{}) -> struct{} {
    ///     // ...
    /// }
    ///
    /// // All of these work without explicit casts:
    /// takes_void_ptr(&x);        // where x: i32
    /// takes_void_ptr(&y);        // where y: SomeStruct
    /// takes_void_ptr(&z);        // where z: bool
    /// ```
    ///
    /// And allows `{int}` to implicitly cast to any integer type:
    /// ```zirco
    /// let x: i8 = 42;    // {int} -> i8
    /// let y: u64 = 100;  // {int} -> u64
    /// ```
    #[must_use]
    pub fn can_implicitly_cast_to(&self, target: &Self) -> bool {
        // Allow any pointer type to implicitly cast to void pointer (*struct{})
        if let (Type::Ptr(_from_pointee), Type::Ptr(to_pointee)) = (self, target)
            && let Type::Struct(fields) = to_pointee.as_ref()
            && fields.is_empty()
        {
            // Target is *struct{}, allow implicit cast from any *T
            return true;
        }

        // Allow arrays to implicitly cast to pointers to their element type
        if let (
            Type::Array {
                element_type: from_element,
                ..
            },
            Type::Ptr(to_pointee),
        ) = (self, target)
            && **to_pointee == **from_element
        {
            return true;
        }

        // Allow {int} to implicitly cast to any concrete integer type
        if matches!(self, Type::Int) && target.is_integer() && !matches!(target, Type::Int) {
            return true;
        }

        // Allow arrays with {int} elements to implicitly cast to arrays with concrete
        // integer elements
        if let (
            Type::Array {
                element_type: from_element,
                size: from_size,
            },
            Type::Array {
                element_type: to_element,
                size: to_size,
            },
        ) = (self, target)
        {
            // Arrays must have the same size (compare numeric values, not string representation)
            if from_size.into_parsed_value() == to_size.into_parsed_value()
                && from_element.can_implicitly_cast_to(to_element)
            {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_void_ptr_implicit_cast() {
        // Create a void pointer type (*struct{})
        let void_ptr = Type::Ptr(Box::new(Type::Struct(IndexMap::new())));

        // Create various pointer types
        let i32_ptr = Type::Ptr(Box::new(Type::I32));
        let bool_ptr = Type::Ptr(Box::new(Type::Bool));
        let struct_ptr = Type::Ptr(Box::new(Type::Struct(IndexMap::from([("x", Type::I8)]))));

        // All should be able to implicitly cast to void pointer
        assert!(i32_ptr.can_implicitly_cast_to(&void_ptr));
        assert!(bool_ptr.can_implicitly_cast_to(&void_ptr));
        assert!(struct_ptr.can_implicitly_cast_to(&void_ptr));

        // Void pointer itself should also work
        assert!(void_ptr.can_implicitly_cast_to(&void_ptr));

        // Non-pointer types should not implicitly cast to void pointer
        assert!(!Type::I32.can_implicitly_cast_to(&void_ptr));

        // Void pointer should not implicitly cast to specific pointer
        assert!(!void_ptr.can_implicitly_cast_to(&i32_ptr));

        // Specific pointers should not implicitly cast to other specific pointers
        assert!(!i32_ptr.can_implicitly_cast_to(&bool_ptr));
    }

    #[test]
    fn type_display_works_for_primitives() {
        assert_eq!(Type::I8.to_string(), "i8");
        assert_eq!(Type::U8.to_string(), "u8");
        assert_eq!(Type::I16.to_string(), "i16");
        assert_eq!(Type::U16.to_string(), "u16");
        assert_eq!(Type::I32.to_string(), "i32");
        assert_eq!(Type::U32.to_string(), "u32");
        assert_eq!(Type::I64.to_string(), "i64");
        assert_eq!(Type::U64.to_string(), "u64");
        assert_eq!(Type::Usize.to_string(), "usize");
        assert_eq!(Type::Isize.to_string(), "isize");
        assert_eq!(Type::Bool.to_string(), "bool");
    }

    #[test]
    fn type_display_works_for_pointer() {
        let ptr_type = Type::Ptr(Box::new(Type::I32));
        assert_eq!(ptr_type.to_string(), "*i32");
    }

    #[test]
    fn type_display_works_for_empty_struct() {
        let struct_type = Type::Struct(IndexMap::new());
        assert_eq!(struct_type.to_string(), "struct {}");
    }

    #[test]
    fn type_display_works_for_empty_union() {
        let union_type = Type::Union(IndexMap::new());
        assert_eq!(union_type.to_string(), "union {}");
    }

    #[test]
    fn into_pointee_returns_pointee_for_pointer() {
        let ptr_type = Type::Ptr(Box::new(Type::I32));
        assert_eq!(ptr_type.into_pointee(), Some(Type::I32));
    }

    #[test]
    fn into_pointee_returns_none_for_non_pointer() {
        assert_eq!(Type::I32.into_pointee(), None);
    }

    #[test]
    fn into_struct_contents_returns_fields_for_struct() {
        let fields = IndexMap::from([("x", Type::I32)]);
        let struct_type = Type::Struct(fields.clone());
        assert_eq!(struct_type.into_struct_contents(), Some(fields));
    }

    #[test]
    fn into_struct_contents_returns_none_for_non_struct() {
        assert_eq!(Type::I32.into_struct_contents(), None);
    }

    #[test]
    fn into_union_contents_returns_fields_for_union() {
        let fields = IndexMap::from([("x", Type::I32)]);
        let union_type = Type::Union(fields.clone());
        assert_eq!(union_type.into_union_contents(), Some(fields));
    }

    #[test]
    fn into_union_contents_returns_none_for_non_union() {
        assert_eq!(Type::I32.into_union_contents(), None);
    }

    #[test]
    fn unit_type_is_empty_struct() {
        let unit = Type::unit();
        match unit {
            Type::Struct(fields) => assert!(fields.is_empty()),
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::Usize
            | Type::Isize
            | Type::Bool
            | Type::Int
            | Type::Ptr(_)
            | Type::Fn(_)
            | Type::Union(_)
            | Type::Opaque(_)
            | Type::Array { .. } => panic!("unit should be an empty struct"),
        }
    }

    #[test]
    fn test_int_type_implicit_cast() {
        let int_type = Type::Int;

        // {int} should implicitly cast to all concrete integer types
        assert!(int_type.can_implicitly_cast_to(&Type::I8));
        assert!(int_type.can_implicitly_cast_to(&Type::U8));
        assert!(int_type.can_implicitly_cast_to(&Type::I16));
        assert!(int_type.can_implicitly_cast_to(&Type::U16));
        assert!(int_type.can_implicitly_cast_to(&Type::I32));
        assert!(int_type.can_implicitly_cast_to(&Type::U32));
        assert!(int_type.can_implicitly_cast_to(&Type::I64));
        assert!(int_type.can_implicitly_cast_to(&Type::U64));
        assert!(int_type.can_implicitly_cast_to(&Type::Isize));
        assert!(int_type.can_implicitly_cast_to(&Type::Usize));

        // {int} should not implicitly cast to non-integer types
        assert!(!int_type.can_implicitly_cast_to(&Type::Bool));
        assert!(!int_type.can_implicitly_cast_to(&Type::Ptr(Box::new(Type::I32))));

        // Concrete integer types should not implicitly cast to {int}
        assert!(!Type::I32.can_implicitly_cast_to(&int_type));

        // {int} should not cast to itself
        assert!(!int_type.can_implicitly_cast_to(&int_type));
    }

    #[test]
    fn test_int_type_is_integer() {
        assert!(Type::Int.is_integer());
        assert!(!Type::Int.is_signed_integer());
        assert!(!Type::Int.is_unsigned_integer());
    }

    #[test]
    fn test_array_with_int_elements_implicit_cast() {
        // Create array types
        let array_int = Type::Array {
            element_type: Box::new(Type::Int),
            size: NumberLiteral::Decimal("2"),
        };
        let array_i32 = Type::Array {
            element_type: Box::new(Type::I32),
            size: NumberLiteral::Decimal("2"),
        };
        let array_u8 = Type::Array {
            element_type: Box::new(Type::U8),
            size: NumberLiteral::Decimal("2"),
        };
        let array_i64 = Type::Array {
            element_type: Box::new(Type::I64),
            size: NumberLiteral::Decimal("2"),
        };

        // [2]{int} should implicitly cast to arrays with concrete integer element types
        assert!(array_int.can_implicitly_cast_to(&array_i32));
        assert!(array_int.can_implicitly_cast_to(&array_u8));
        assert!(array_int.can_implicitly_cast_to(&array_i64));

        // Arrays with concrete types should not cast to {int} arrays
        assert!(!array_i32.can_implicitly_cast_to(&array_int));

        // Arrays with different sizes should not cast
        let array_int_3 = Type::Array {
            element_type: Box::new(Type::Int),
            size: NumberLiteral::Decimal("3"),
        };
        assert!(!array_int_3.can_implicitly_cast_to(&array_i32));

        // Arrays with different concrete element types should not cast
        assert!(!array_i32.can_implicitly_cast_to(&array_u8));

        // Test with different number representations (hex vs decimal)
        let array_hex_10 = Type::Array {
            element_type: Box::new(Type::Int),
            size: NumberLiteral::Hexadecimal("A"), // 0xA = 10
        };
        let array_decimal_10 = Type::Array {
            element_type: Box::new(Type::I32),
            size: NumberLiteral::Decimal("10"),
        };
        // These should be equal (both represent size 10)
        assert!(array_hex_10.can_implicitly_cast_to(&array_decimal_10));
    }
}
