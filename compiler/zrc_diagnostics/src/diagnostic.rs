//! Defines types for Zirco compile time diagnostics.
//!
//! This module defines the structure and kinds of diagnostics
//! that can be produced during the compilation process. It includes
//! the [`Diagnostic`] struct, which encapsulates a diagnostic message along
//! with its severity and span.

use std::{
    error::Error,
    fmt::{Debug, Display},
};

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

    /// Warning. Compilation may continue.
    #[display("warning")]
    Warning,
}

impl Severity {
    /// Convert severity to ariadne's [`ReportKind`]
    const fn to_report_kind(&self) -> ReportKind<'static> {
        match *self {
            Self::Error => ReportKind::Error,
            Self::Warning => ReportKind::Warning,
        }
    }

    /// Get the color for this severity
    const fn color(&self) -> Color {
        match *self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
        }
    }
}

/// A diagnostic message produced by one of the zrc tools
#[derive(Debug, PartialEq, Eq, Display)]
#[display("{_0}: {_1}")]
pub struct GenericDiagnostic<Kind>(pub Severity, pub Spanned<Kind>)
where
    Kind: Debug + PartialEq + Eq + Display;

impl<K: Debug + PartialEq + Eq + Display> GenericDiagnostic<K> {
    /// Convert this [`Diagnostic`] to a printable string using ariadne. The
    /// source code is provided directly as a string if it is not read from a
    /// file.
    ///
    /// # Panics
    /// This function may panic if the span is invalid or if writing to the
    /// buffer fails.
    #[must_use]
    pub fn print(&self, piped_source: Option<&str>) -> String {
        let span = self.1.span();
        let message = self.1.to_string();

        // read the source from the path inside of the span. if it is <stdin>, use
        // the provided piped source.
        let (source, path) = match span.file_name() {
            "<stdin>" => (
                piped_source
                    .expect("piped source must be provided for <stdin>")
                    .to_string(),
                "<stdin>",
            ),
            path => {
                let source = std::fs::read_to_string(path)
                    .unwrap_or_else(|_| panic!("failed to read source file {path} for diagnostic"));
                (source, path)
            }
        };

        // Create ariadne report using (filename, range) as the span type
        let report = Report::build(self.0.to_report_kind(), (path, span.start()..span.end()))
            .with_message(message.clone())
            .with_label(
                Label::new((path, span.start()..span.end()))
                    .with_message(message)
                    .with_color(self.0.color()),
            )
            .finish();

        // Write report to string
        let mut buffer = Vec::new();
        report
            .write((path, ariadne::Source::from(source)), &mut buffer)
            .expect("failed to write diagnostic");

        String::from_utf8(buffer).expect("diagnostic output should be valid UTF-8")
    }
}
impl<K: Debug + PartialEq + Eq + Display> Error for GenericDiagnostic<K> {}

/// A diagnostic message produced by the Zirco compiler
#[derive(Debug, PartialEq, Eq, Display)]
#[display("{_0}: {_1}")]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);
impl Diagnostic {
    /// Get a generic version of this diagnostic
    #[must_use]
    pub fn as_generic(&self) -> GenericDiagnostic<DiagnosticKind> {
        GenericDiagnostic(self.0.clone(), self.1.clone())
    }

    /// Convert this [`Diagnostic`] to a printable string using ariadne. The
    /// source code is provided directly as a string if it is not read from a
    /// file.
    ///
    /// # Panics
    /// This function may panic if the span is invalid or if writing to the
    /// buffer fails.
    #[must_use]
    pub fn print(&self, piped_source: Option<&str>) -> String {
        self.as_generic().print(piped_source)
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::{spanned, spanned_test};

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
            spanned_test!(0, DiagnosticKind::InvalidToken, 4),
        );
        let display = diagnostic.to_string();
        assert!(display.contains("error"));
        assert!(display.contains("invalid token"));
    }

    #[test]
    fn diagnostic_print_formats_correctly() {
        let source = "let x = 5;";
        let diagnostic = Diagnostic(
            Severity::Error,
            spanned!(4, DiagnosticKind::InvalidToken, 5, "<stdin>"),
        );
        let output = diagnostic.print(Some(source));

        assert!(output.contains("<stdin>"));
        assert!(output.contains("Error"));
        assert!(output.contains("invalid token"));
    }
}
