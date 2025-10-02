//! Associations to spans within the input source
//!
//! This module declares two very helpful types, [`Span`] and [`Spanned<T>`],
//! which both help the compiler associate things like tokens with locations
//! within the source file. There is also a trait [`Spannable`] which allows
//! some easier chained method calls.
//!
//! Read the documentation for the types [`Span`] and [`Spanned<T>`], and the
//! trait [`Spannable`] to learn more.

use std::{fmt::Display, ops::RangeInclusive};

/// Represents the start and end of some segment of a string
///
/// A span represents the start and end of some span within a string. It can be
/// sliced into a str, and various other operations. You can also create a
/// [`Spanned<T>`] from a span using the [`Span::containing`] method.
///
/// These are often found within the [`Spanned<T>`] type. They can be obtained
/// in a few ways:
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
        assert!(
            end >= start,
            "span must have positive length (got span {start}..{end})"
        );
        Self(start, end)
    }

    /// Obtains the starting position of this [`Span`] as a `usize`
    #[must_use]
    #[inline]
    pub const fn start(&self) -> usize {
        self.0
    }

    /// Obtains the ending position of this [`Span`] as a `usize`
    #[must_use]
    #[inline]
    pub const fn end(&self) -> usize {
        self.1
    }

    /// Convert this [`Span`] into a [`RangeInclusive`], good for slicing into
    /// your input
    #[must_use]
    #[inline]
    pub const fn range(&self) -> RangeInclusive<usize> {
        self.start()..=self.end()
    }

    /// Creates a [`Spanned<T>`] instance using this [`Span`], a passed value,
    /// and a file name
    #[must_use]
    #[inline]
    pub const fn containing<T>(self, value: T, file_name: &'static str) -> Spanned<T> {
        Spanned::from_span_and_value(self, value, file_name)
    }

    /// Creates a new [`Span`] containing the intersection of two passed spans
    ///
    /// If this returns [`None`], no intersection exists (they are disjoint).
    #[must_use]
    pub fn intersect(span_a: Self, span_b: Self) -> Option<Self> {
        if span_a.start() > span_b.end() || span_b.start() > span_a.end() {
            None
        } else {
            Some(Self::from_positions(
                std::cmp::max(span_a.start(), span_b.start()),
                std::cmp::min(span_a.end(), span_b.end()),
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
/// These are often found in many places throughout the compiler, such as
/// attached to AST nodes or in diagnostics.
///
/// An instance of [`Spanned`] can be obtained in a few ways:
/// - Direct construction from a [`Span`] and a value
///   ([`Spanned::from_span_and_value`])
/// - By attaching a value to a [`Span`] ([`Span::containing`])
/// - By attaching a [`Span`] to a value (with the [`Spannable`] trait's
///   [`Spannable::in_span`] method)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T>(Span, T, &'static str);
impl<T> Spanned<T> {
    /// Create a new [`Spanned<T>`] instance from a [`Span`], value, and file name
    #[must_use]
    #[inline]
    pub const fn from_span_and_value(span: Span, value: T, file_name: &'static str) -> Self {
        Self(span, value, file_name)
    }

    /// Obtains the [`Span`] associated with this [`Spanned<T>`] instance
    #[must_use]
    #[inline]
    pub const fn span(&self) -> Span {
        self.0
    }

    /// Obtains a reference to the value this [`Spanned<T>`] instance wraps
    #[inline]
    pub const fn value(&self) -> &T {
        &self.1
    }

    /// Obtains the file name associated with this [`Spanned<T>`] instance
    #[must_use]
    #[inline]
    pub const fn file_name(&self) -> &'static str {
        self.2
    }

    /// Applies a function to the contained value, returning a new
    /// [`Spanned<T>`] instance with the same associated [`Span`] and file name
    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let span = self.span();
        let file_name = self.file_name();
        Spanned::from_span_and_value(span, f(self.into_value()), file_name)
    }

    /// "Strips" the [`Spanned<T>`] of its [`Span`], returning the inner value
    ///
    /// This differs from [`Spanned::value`] because it consumes the
    /// [`Spanned<T>`] instance and drops the [`Span`].
    #[allow(clippy::missing_const_for_fn)]
    #[inline]
    pub fn into_value(self) -> T {
        self.1
    }

    /// Obtains the starting position of the contained [`Span`] as a `usize`
    #[must_use]
    #[inline]
    pub const fn start(&self) -> usize {
        self.span().start()
    }

    /// Obtains the ending position of the contained [`Span`] as a `usize`
    #[must_use]
    #[inline]
    pub const fn end(&self) -> usize {
        self.span().end()
    }

    /// Converts a [`&Spanned<T>`][Spanned] to a [`Spanned<&T>`].
    #[inline]
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned::from_span_and_value(self.span(), &self.1, self.file_name())
    }
}
impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value().fmt(f)
    }
}
impl<T> Spanned<Option<T>> {
    /// Converts a [`Spanned<Option<T>>`] to a [`Option<Spanned<T>>`].
    /// Note: This is not reversible because if you wanted to create a
    /// [`Spanned`] [`Some`] from [`None`], what span would you use?
    pub fn transpose(self) -> Option<Spanned<T>> {
        let span = self.span();
        let file_name = self.file_name();
        self.into_value().map(|x| Spanned::from_span_and_value(span, x, file_name))
    }
}
impl<T, E> Spanned<Result<T, E>> {
    /// Converts a [`Spanned<Result<T, E>>`] to a [`Result<Spanned<T>,
    /// Spanned<E>>`]. Note: This is not reversible. See the note on
    /// [`Spanned<Option<T>>::transpose`].
    #[allow(clippy::missing_errors_doc)] // just propagates input error
    pub fn transpose(self) -> Result<Spanned<T>, Spanned<E>> {
        let span = self.span();
        let file_name = self.file_name();
        self.into_value()
            .map(|x| Spanned::from_span_and_value(span, x, file_name))
            .map_err(|x| Spanned::from_span_and_value(span, x, file_name))
    }
}

/// A trait automatically implemented on all types that allows you to attach a
/// [`Span`] to something, creating a [`Spanned<T>`] instance.
pub trait Spannable
where
    Self: Sized,
{
    /// Attach a [`Span`] and file name to this value, creating a [`Spanned<T>`] instance
    ///
    /// This method can be used to attach a [`Span`] and file name to any arbitrary value. It
    /// is a cleaner syntax for the [`Spanned::from_span_and_value`] or
    /// [`Span::containing`] functions.
    fn in_span(self, span: Span, file_name: &'static str) -> Spanned<Self>;
}

// Automatically implement Spannable for all types
impl<T: Sized> Spannable for T {
    #[inline]
    fn in_span(self, span: Span, file_name: &'static str) -> Spanned<Self> {
        Spanned::from_span_and_value(span, self, file_name)
    }
}

/// Create a [`Spanned<T>`] instance from two locations, a value, and a file name.
/// Simply just expands to a [`Spanned::from_span_and_value`] and
/// [`Span::from_positions`] calls.
///
/// # Panics
/// Panics if `start > end`.
#[macro_export]
macro_rules! spanned {
    ($start:expr, $value:expr, $end:expr, $file_name:expr) => {
        $crate::span::Spanned::from_span_and_value(
            $crate::span::Span::from_positions($start, $end),
            $value,
            $file_name,
        )
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spanned_macro_creates_spanned_item() {
        assert_eq!(
            spanned!(0, (), 3, "test.zrc"),
            Spanned::from_span_and_value(Span::from_positions(0, 3), (), "test.zrc")
        );
    }

    mod span {
        use super::*;

        #[test]
        fn span_from_positions_works_as_expected() {
            let span = Span::from_positions(2, 7);
            assert_eq!(span, Span(2, 7));
            assert_eq!(span.start(), 2);
            assert_eq!(span.end(), 7);
            assert_eq!(span.range(), 2..=7);
            assert_eq!(span.to_string(), "2-7".to_string());
        }

        #[test]
        #[allow(clippy::let_underscore_must_use)]
        #[should_panic(expected = "span must have positive length")]
        fn span_from_invalid_positions_panics() {
            let _ = Span::from_positions(5, 0);
        }

        #[test]
        fn span_intersection_with_overlap_returns_intersection() {
            assert_eq!(
                Span::intersect(Span::from_positions(0, 6), Span::from_positions(4, 10)),
                Some(Span::from_positions(4, 6))
            );
        }

        #[test]
        fn span_intersections_with_disjoint_spans_returns_none() {
            assert_eq!(
                Span::intersect(Span::from_positions(0, 5), Span::from_positions(7, 10)),
                None
            );
        }

        #[test]
        fn span_containing_returns_spanned() {
            assert_eq!(
                Span::from_positions(0, 3).containing((), "test.zrc"),
                spanned!(0, (), 3, "test.zrc")
            );
        }
    }

    mod spanned {
        use super::*;

        #[test]
        fn basic_methods_work_as_expected() {
            let span = Span::from_positions(3, 6);
            let spanned = Spanned::from_span_and_value(span, 0, "test.zrc");

            assert_eq!(spanned.span(), span);
            assert_eq!(spanned.start(), 3);
            assert_eq!(spanned.end(), 6);
            assert_eq!(spanned.value(), &0);
            assert_eq!(spanned.file_name(), "test.zrc");
            assert_eq!(spanned.into_value(), 0);
        }

        #[test]
        fn map_works_as_expected() {
            let spanned = spanned!(3, 0, 6, "test.zrc");

            assert_eq!(spanned.map(|n| n == 0), spanned!(3, true, 6, "test.zrc"));
        }

        #[test]
        fn as_ref_works_as_expected() {
            let spanned = spanned!(3, 0, 6, "test.zrc");

            assert_eq!(spanned.as_ref(), spanned!(3, &0, 6, "test.zrc"));
        }

        #[test]
        fn transpose_option_some_case() {
            let spanned = spanned!(3, Some(0), 6, "test.zrc");

            assert_eq!(spanned.transpose(), Some(spanned!(3, 0, 6, "test.zrc")));
        }

        #[test]
        fn transpose_option_none_case() {
            let spanned: Spanned<Option<()>> = spanned!(3, None, 6, "test.zrc");

            assert_eq!(spanned.transpose(), None);
        }

        #[test]
        fn transpose_result_ok_case() {
            let spanned: Spanned<Result<i32, ()>> = spanned!(3, Ok(0), 6, "test.zrc");

            assert_eq!(spanned.transpose(), Ok(spanned!(3, 0, 6, "test.zrc")));
        }

        #[test]
        fn transpose_result_err_case() {
            let spanned: Spanned<Result<(), i32>> = spanned!(3, Err(0), 6, "test.zrc");

            assert_eq!(spanned.transpose(), Err(spanned!(3, 0, 6, "test.zrc")));
        }
    }

    #[test]
    fn spannable_in_span_creates_spanned() {
        assert_eq!(
            7.in_span(Span::from_positions(3, 6), "test.zrc"),
            Spanned(Span(3, 6), 7, "test.zrc"),
        );
    }
}
