//! Type representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Type`] struct. It
//! contains all the different type kinds in Zirco.

use std::fmt::Display;

use zrc_utils::{
    span::{Span, Spannable, Spanned},
    spanned,
};

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Type(pub Spanned<TypeKind>);

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeKind {
    /// An identifier, such as `i32`
    Identifier(String),
    /// `*T`
    Ptr(Box<Type>),
    /// A direct struct type
    #[allow(clippy::type_complexity)]
    Struct(Spanned<Vec<Spanned<(Spanned<String>, Type)>>>),
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(i) => write!(f, "{i}"),
            Self::Ptr(t) => write!(f, "*{t}"),
            Self::Struct(members) => {
                write!(f, "struct {{ ")?;
                for (i, m) in members.value().iter().enumerate() {
                    write!(f, "{}: {}", m.value().0.value(), m.value().1)?;
                    if i < members.value().len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}

// AST builder. We are able to infer the spans of many based on the start of
// their leftmost and the end of their rightmost operands.
#[allow(missing_docs)]
#[allow(clippy::missing_docs_in_private_items)]
#[allow(clippy::should_implement_trait)]
impl Type {
    #[must_use]
    pub fn ident(ident: Spanned<String>) -> Self {
        let span = ident.span();
        Self(spanned!(
            span.start(),
            TypeKind::Identifier(ident.into_value()),
            ident.end()
        ))
    }

    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn struct_direct(span: Span, keys: Spanned<Vec<Spanned<(Spanned<String>, Self)>>>) -> Self {
        Self(TypeKind::Struct(keys).in_span(span))
    }

    #[must_use]
    pub fn ptr(span: Span, ty: Self) -> Self {
        Self(TypeKind::Ptr(Box::new(ty)).in_span(span))
    }
}
