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
pub struct Type<'input>(pub Spanned<TypeKind<'input>>);

/// A valid Zirco AST type
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeKind<'input> {
    /// An identifier, such as `i32`
    Identifier(&'input str),
    /// `*T`
    Ptr(Box<Type<'input>>),
    /// A direct struct type
    #[allow(clippy::type_complexity)]
    Struct(Spanned<Vec<Spanned<(Spanned<&'input str>, Type<'input>)>>>),
}

impl<'input> Display for TypeKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(i) => write!(f, "{i}"),
            Self::Ptr(pointee_ty) => write!(f, "*{pointee_ty}"),
            Self::Struct(members) => {
                write!(f, "struct {{ ")?;
                for (i, member) in members.value().iter().enumerate() {
                    write!(f, "{}: {}", member.value().0.value(), member.value().1)?;
                    if i < members.value().len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
impl<'input> Display for Type<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}

// AST builder. We are able to infer the spans of many based on the start of
// their leftmost and the end of their rightmost operands.
#[allow(missing_docs)]
#[allow(clippy::missing_docs_in_private_items)]
#[allow(clippy::should_implement_trait)]
impl<'input> Type<'input> {
    #[must_use]
    pub fn ident(ident: Spanned<&'input str>) -> Self {
        let span = ident.span();
        Self(spanned!(
            span.start(),
            TypeKind::Identifier(ident.into_value()),
            ident.end()
        ))
    }

    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn struct_direct(
        span: Span,
        keys: Spanned<Vec<Spanned<(Spanned<&'input str>, Self)>>>,
    ) -> Self {
        Self(TypeKind::Struct(keys).in_span(span))
    }

    #[must_use]
    pub fn ptr(span: Span, ty: Self) -> Self {
        Self(TypeKind::Ptr(Box::new(ty)).in_span(span))
    }
}
