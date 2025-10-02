//! Extension types for Zirco diagnostics

use zrc_utils::span::Spanned;

use crate::{Diagnostic, DiagnosticKind, Severity};

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
    use zrc_utils::{span::Span, spanned};

    use super::*;
    use crate::DiagnosticKind;

    #[test]
    fn diagnostic_kind_error_in_creates_diagnostic() {
        let span = Span::from_positions(0, 5);
        let diagnostic = DiagnosticKind::InvalidToken.error_in(span, "test.zrc");

        assert_eq!(diagnostic.0, Severity::Error);
        assert_eq!(diagnostic.1.span(), span);
        assert_eq!(diagnostic.1.file_name(), "test.zrc");
    }

    #[test]
    fn spanned_ext_creates_diagnostic() {
        let spanned_str = spanned!(0, "test", 4, "test.zrc");
        let diagnostic =
            spanned_str.error(|value| DiagnosticKind::UnableToResolveIdentifier(value.to_string()));

        assert_eq!(diagnostic.0, Severity::Error);
    }
}
