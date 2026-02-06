//! Extension types for Zirco diagnostics

use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{Diagnostic, DiagnosticKind};

/// A trait to easily create [`Diagnostic`]s from [`Span`]s
/// See also: [`Spannable`]
pub trait SpanExt {
    /// Create a [`Diagnostic`] from this [`Span`] and a [`DiagnosticKind`]
    #[must_use]
    fn error(self, kind: DiagnosticKind) -> Diagnostic;
}
impl SpanExt for Span {
    #[inline]
    fn error(self, kind: DiagnosticKind) -> Diagnostic {
        Diagnostic::error(kind.in_span(self))
    }
}

/// A trait to easily create [`Diagnostic`]s from [`Spanned`]s
/// See also: [`SpanExt`] and [`Spannable`]
pub trait SpannedExt<T> {
    /// Create a [`Diagnostic`] from this [`Spanned`] and a [`DiagnosticKind`]
    #[must_use]
    fn error(self, f: impl Fn(T) -> DiagnosticKind) -> Diagnostic;
}
impl<T> SpannedExt<T> for Spanned<T> {
    #[inline]
    fn error(self, f: impl Fn(T) -> DiagnosticKind) -> Diagnostic {
        Diagnostic::error(self.map(f))
    }
}
