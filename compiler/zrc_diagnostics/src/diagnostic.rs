//! Defines types for Zirco compile time diagnostics.
//!
//! This module defines the structure and kinds of diagnostics
//! that can be produced during the compilation process. It includes
//! the [`Diagnostic`] struct, which encapsulates a diagnostic message along
//! with its severity and span.

use std::{error::Error, path::Path};

use ariadne::{Color, Label, Report, ReportKind};
use derive_more::Display;
use zrc_utils::span::Spanned;

use crate::DiagnosticKind;

/// The severity of a [`Diagnostic`].
#[derive(Clone, PartialEq, Eq, Debug, Display)]
pub enum Severity {
    /// Error. Compilation will not continue.
    #[display("error")]
    Error,
}

impl Severity {
    /// Convert severity to ariadne's [`ReportKind`]
    const fn to_report_kind(&self) -> ReportKind<'static> {
        match *self {
            Self::Error => ReportKind::Error,
        }
    }

    /// Get the color for this severity
    const fn color(&self) -> Color {
        match *self {
            Self::Error => Color::Red,
        }
    }
}

/// A diagnostic message produced by zrc
#[derive(Debug, PartialEq, Eq, Display)]
#[display("{_0}: {_1}")]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);

impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string using ariadne with a
    /// custom filename
    ///
    /// # Panics
    /// This function may panic if the span is invalid or if writing to the
    /// buffer fails.
    #[must_use]
    pub fn print_with_filename(&self, source: &str, path: &Path) -> String {
        let span = self.1.span();
        let message = self.1.to_string();

        // Create ariadne report using (filename, range) as the span type
        let filename = path.to_string_lossy();
        let report = Report::build(
            self.0.to_report_kind(),
            (filename.as_ref(), span.start()..span.end()),
        )
        .with_message(message.clone())
        .with_label(
            Label::new((filename.as_ref(), span.start()..span.end()))
                .with_message(message)
                .with_color(self.0.color()),
        )
        .finish();

        // Write report to string
        let mut buffer = Vec::new();
        report
            .write(
                (filename.as_ref(), ariadne::Source::from(source)),
                &mut buffer,
            )
            .expect("failed to write diagnostic");

        String::from_utf8(buffer).expect("diagnostic output should be valid UTF-8")
    }
}

impl Error for Diagnostic {}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;
    use crate::DiagnosticKind;

    #[test]
    fn severity_display_works_correctly() {
        assert_eq!(Severity::Error.to_string(), "error");
    }

    #[test]
    fn severity_to_report_kind_returns_error() {
        assert_eq!(Severity::Error.to_report_kind(), ReportKind::Error);
    }

    #[test]
    fn severity_color_returns_red() {
        assert_eq!(Severity::Error.color(), Color::Red);
    }

    #[test]
    fn diagnostic_display_includes_severity_and_kind() {
        let diagnostic = Diagnostic(
            Severity::Error,
            spanned!(0, DiagnosticKind::InvalidToken, 4),
        );
        let display = diagnostic.to_string();
        assert!(display.contains("error"));
        assert!(display.contains("invalid token"));
    }

    #[test]
    fn diagnostic_print_with_filename_formats_correctly() {
        let source = "let x = 5;";
        let diagnostic = Diagnostic(
            Severity::Error,
            spanned!(4, DiagnosticKind::InvalidToken, 5),
        );
        let path = Path::new("test.zrc");
        let output = diagnostic.print_with_filename(source, path);

        assert!(output.contains("test.zrc"));
        assert!(output.contains("Error"));
        assert!(output.contains("invalid token"));
    }
}
