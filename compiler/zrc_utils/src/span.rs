//! Associations to spans within the input source
//!
//! This module declares two very helpful types, [`Span`] and [`Spanned<T>`], which both help the
//! compiler associate things like tokens with locations within the source file. There is also a
//! trait [`Spannable`] which allows some easier chained method calls.
//!
//! Read the documentation for the types [`Span`] and [`Spanned<T>`], and the trait [`Spannable`]
//! to learn more.

use std::{fmt::Display, ops::RangeInclusive};

/// Represents the start and end of some segment of a string
///
/// A span represents the start and end of some span within a string. It can be sliced into a str,
/// and various other operations. You can also create a [`Spanned<T>`] from a span using the
/// [`Span::containing`] method.
///
/// These are often found within the [`Spanned<T>`] type. They can be obtained in a few ways:
/// - Direct construction ([`Span::from_positions`])
/// - Methods on another Span ([`Span::intersect`])
/// - Stripping the value from a [`Spanned<T>`] ([`Spanned::span`])
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(usize, usize);
impl Span {
    /// Create a new [`Span`] given a start and end location.
    ///
    /// # Panics
    /// Panics if `start > end`.
    #[must_use]
    pub fn from_positions(start: usize, end: usize) -> Self {
        assert!(end >= start);
        Self(start, end)
    }

    /// Obtains the starting position of this [`Span`] as a `usize`
    #[must_use]
    pub const fn start(&self) -> usize {
        self.0
    }

    /// Obtains the ending position of this [`Span`] as a `usize`
    #[must_use]
    pub const fn end(&self) -> usize {
        self.1
    }

    /// Convert this [`Span`] into a [`RangeInclusive`], good for slicing into your input
    #[must_use]
    pub const fn range(&self) -> RangeInclusive<usize> {
        self.start()..=self.end()
    }

    /// Creates a [`Spanned<T>`] instance using this [`Span`] and a passed value
    #[must_use]
    pub const fn containing<T>(self, value: T) -> Spanned<T> {
        Spanned::from_span_and_value(self, value)
    }

    /// Creates a new [`Span`] containing the intersection of two passed spans
    ///
    /// If this returns [`None`], no intersection exists (they are disjoint).
    #[must_use]
    pub fn intersect(a: Self, b: Self) -> Option<Self> {
        if a.start() > b.end() || b.start() > a.end() {
            None
        } else {
            Some(Self::from_positions(
                std::cmp::max(a.start(), b.start()),
                std::cmp::min(a.end(), b.end()),
            ))
        }
    }
}
impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start(), self.end())
    }
}

/// Represents something (`T`) contained within a [`Span`].
///
/// These are often found in many places throughout the compiler, such as attached to AST nodes or
/// in diagnostics.
///
/// An instance of [`Spanned`] can be obtained in a few ways:
/// - Direct construction from a [`Span`] and a value ([`Spanned::from_span_and_value`])
/// - By attaching a value to a [`Span`] ([`Span::containing`])
/// - By attaching a [`Span`] to a value (with the [`Spannable`] trait's [`Spannable::in_span`] method)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T>(Span, T);
impl<T> Spanned<T> {
    /// Create a new [`Spanned<T>`] instance from a [`Span`] and some value
    #[must_use]
    pub const fn from_span_and_value(span: Span, value: T) -> Self {
        Self(span, value)
    }

    /// Obtains the [`Span`] associated with this [`Spanned<T>`] instance
    #[must_use]
    pub const fn span(&self) -> Span {
        self.0
    }

    /// Obtains a reference to the value this [`Spanned<T>`] instance wraps
    pub const fn value(&self) -> &T {
        &self.1
    }

    /// Applies a function to the contained value, returning a new [`Spanned<T>`] instance with the
    /// same associated [`Span`]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        self.span().containing(f(self.into_value()))
    }

    /// "Strips" the [`Spanned<T>`] of its [`Span`], returning the inner value
    ///
    /// This differs from [`Spanned::value`] because it consumes the [`Spanned<T>`] instance and
    /// drops the [`Span`].
    #[allow(clippy::missing_const_for_fn)]
    pub fn into_value(self) -> T {
        self.1
    }

    /// Obtains the starting position of the contained [`Span`] as a `usize`
    #[must_use]
    pub const fn start(&self) -> usize {
        self.span().start()
    }

    /// Obtains the ending position of the contained [`Span`] as a `usize`
    #[must_use]
    pub const fn end(&self) -> usize {
        self.span().end()
    }
}

/// A trait automatically implemented on all types that allows you to attach a [`Span`] to something,
/// creating a [`Spanned<T>`] instance.
pub trait Spannable
where
    Self: Sized,
{
    /// Attach a [`Span`] to this value, creating a [`Spanned<T>`] instance
    ///
    /// This method can be used to attach a [`Span`] to any arbitrary value. It is a cleaner syntax
    /// for the [`Spanned::from_span_and_value`] or [`Span::containing`] functions.
    fn in_span(self, span: Span) -> Spanned<Self>;
}

// Automatically implement Spannable for all types
impl<T: Sized> Spannable for T {
    fn in_span(self, span: Span) -> Spanned<Self> {
        Spanned::from_span_and_value(span, self)
    }
}
