//! Type representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Type`] struct. It
//! contains all the different type kinds in Zirco.

use std::fmt::Display;

use derive_more::Display;
use zrc_utils::{
    span::{Span, Spannable, Spanned},
    spanned,
};

use crate::{ast::stmt::ArgumentDeclarationList, lexer::NumberLiteral};

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone, Display)]
#[display("{_0}")]
pub struct Type<'input>(pub Spanned<TypeKind<'input>>);

/// The key-value pairs of a struct
#[derive(PartialEq, Eq, Debug, Clone)]
#[expect(clippy::type_complexity)]
pub struct KeyTypeMapping<'input>(pub Spanned<Vec<Spanned<(Spanned<&'input str>, Type<'input>)>>>);
impl Display for KeyTypeMapping<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, member) in self.0.value().iter().enumerate() {
            write!(f, "{}: {}", member.value().0.value(), member.value().1)?;
            if i < self.0.value().len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum TypeKind<'input> {
    /// An identifier, such as `i32`
    #[display("{_0}")]
    Identifier(&'input str),
    /// `*T`
    #[display("*{_0}")]
    Ptr(Box<Type<'input>>),
    /// A direct struct type
    #[display("struct {{ {_0} }}")]
    Struct(KeyTypeMapping<'input>),
    /// A direct union type
    #[display("union {{ {_0} }}")]
    Union(KeyTypeMapping<'input>),
    /// A tagged union type
    #[display("enum {{ {_0} }}")]
    Enum(KeyTypeMapping<'input>),
    /// An array type
    #[display("[{size}]{element_type}")]
    Array {
        /// The element type
        /// e.g. `i32` in `[4]i32`
        element_type: Box<Type<'input>>,
        /// The size integer
        size: NumberLiteral<'input>,
    },
    /// A function type
    /// `fn(params) -> return_type`
    #[display("fn({parameters}) -> {return_type}")]
    Function {
        /// The function parameters
        parameters: Box<ArgumentDeclarationList<'input>>,
        /// The return type, if any
        return_type: Box<Type<'input>>,
    },
}

// AST builder. We are able to infer the spans of many based on the start of
// their leftmost and the end of their rightmost operands.
#[allow(missing_docs, clippy::missing_docs_in_private_items)]
impl<'input> Type<'input> {
    #[must_use]
    pub fn build_ident(ident: Spanned<&'input str>) -> Self {
        let span = ident.span();
        Self(spanned!(
            span.start(),
            TypeKind::Identifier(ident.into_value()),
            ident.end(),
            span.file_name()
        ))
    }

    #[must_use]
    pub fn build_struct_from_contents(span: Span, keys: KeyTypeMapping<'input>) -> Self {
        Self(TypeKind::Struct(keys).in_span(span))
    }

    #[must_use]
    pub fn build_union_from_contents(span: Span, keys: KeyTypeMapping<'input>) -> Self {
        Self(TypeKind::Union(keys).in_span(span))
    }

    #[must_use]
    pub fn build_enum_from_contents(span: Span, keys: KeyTypeMapping<'input>) -> Self {
        Self(TypeKind::Enum(keys).in_span(span))
    }

    #[must_use]
    pub fn build_ptr(span: Span, ty: Self) -> Self {
        Self(TypeKind::Ptr(Box::new(ty)).in_span(span))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn types_stringify_to_their_canonical_form() {
        // A list of sample types in "canonical form."
        // These are parsed then stringified again, and tested for equality.

        let test_cases = vec![
            "i32",
            "*i32",
            "struct { a: i32, b: i32 }",
            "union { a: i32, b: i32 }",
            "enum { Eight: i8, Sixteen: i16 }",
            "fn(x: i32, y: i32) -> i32",
        ];

        for input in test_cases {
            assert_eq!(
                crate::parser::parse_type(input, "<test>")
                    .expect("test cases should have parsed correctly")
                    .to_string(),
                input
            );
        }
    }

    #[test]
    fn parenthesized_types_parse_correctly() {
        // Parenthesized types should parse and normalize to their canonical form
        // (without unnecessary parentheses)

        let test_cases = vec![
            ("*(i32)", "*i32"),
            ("**(i32)", "**i32"),
            ("*(struct { a: i32 })", "*struct { a: i32 }"),
            ("*(union { a: i32, b: i32 })", "*union { a: i32, b: i32 }"),
            // Nested parentheses
            ("*((i32))", "*i32"),
            ("*((struct { a: i32 }))", "*struct { a: i32 }"),
            // Parenthesized types in struct fields
            ("struct { x: (i32) }", "struct { x: i32 }"),
        ];

        for (input, expected) in test_cases {
            let parsed = crate::parser::parse_type(input, "<test>")
                .unwrap_or_else(|_| panic!("Failed to parse: {input}"));
            assert_eq!(
                parsed.to_string(),
                expected,
                "Input: {input} should normalize to: {expected}"
            );
        }
    }

    #[test]
    fn type_aliases_support_parenthesized_types() {
        // Type aliases should support parenthesized types including nested parentheses
        let test_cases = vec!["type x = (i32);", "type y = ((i32));", "type z = (*(i32));"];

        for input in test_cases {
            let result = crate::parser::parse_program(input, "<test>");
            assert!(result.is_ok(), "Failed to parse type alias: {input}");
        }
    }
}
