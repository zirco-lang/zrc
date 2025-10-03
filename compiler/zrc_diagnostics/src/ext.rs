//! Extension types for Zirco diagnostics

use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{Diagnostic, DiagnosticKind, Severity};

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
        Diagnostic(Severity::Error, kind.in_span(self, "<unknown>"))
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
        Diagnostic(Severity::Error, self.map(f))
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;

    #[test]
    fn span_ext_creates_diagnostic() {
        let span = Span::from_positions(0, 5);
        let diagnostic = span.error(DiagnosticKind::InvalidToken);

        assert_eq!(diagnostic.0, Severity::Error);
        assert_eq!(diagnostic.1.span(), span);
    }

    #[test]
    fn spanned_ext_creates_diagnostic() {
        let spanned_str = spanned!(0, "test", 4, "test.zrc");
        let diagnostic =
            spanned_str.error(|value| DiagnosticKind::UnableToResolveIdentifier(value.to_string()));

        assert_eq!(diagnostic.0, Severity::Error);
    }
}
