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
use serde::Serialize;
use zrc_utils::span::Spanned;

use crate::{
    DiagnosticKind,
    diagnostic_kind::{HelpKind, LabelKind, NoteKind},
};

/// The format to emit diagnostics in
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticEmitFormat {
    /// Human-readable text format (default)
    Human,
    /// JSON format for machine consumption
    Json,
}

/// The severity of a [`Diagnostic`].
#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
pub enum Severity {
    /// Error. Compilation will not continue.
    Error,

    /// Warning. Compilation may continue.
    Warning,
}
impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
        }
    }
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
    #[allow(unused)] // i'm sure we'll need this later
    const fn color(&self) -> Color {
        match *self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
        }
    }
}

/// The severity of a [`Label`].
#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
pub enum LabelType {
    /// Error. Compilation will not continue.
    Error,

    /// Warning. Compilation may continue.
    Warning,

    /// Note. Additional information.
    Note,
}
impl LabelType {
    /// Get the color for this severity
    const fn color(&self) -> Color {
        match *self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
            Self::Note => Color::Cyan,
        }
    }
}

/// A label in the diagnostic window
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct GenericLabel<LK>
where
    LK: Debug + PartialEq + Eq + Display + Serialize,
{
    /// The severity of this label
    pub severity: LabelType,
    /// The span and kind of this label
    pub kind: Spanned<LK>,
}
impl<LK> GenericLabel<LK>
where
    LK: Debug + PartialEq + Eq + Display + Serialize,
{
    /// Create a new label with the given severity and kind.
    pub const fn new(severity: LabelType, kind: Spanned<LK>) -> Self {
        Self { severity, kind }
    }

    /// Create a new error label.
    pub const fn error(kind: Spanned<LK>) -> Self {
        Self::new(LabelType::Error, kind)
    }

    /// Create a new warning label.
    pub const fn warning(kind: Spanned<LK>) -> Self {
        Self::new(LabelType::Warning, kind)
    }

    /// Create a new note label.
    pub const fn note(kind: Spanned<LK>) -> Self {
        Self::new(LabelType::Note, kind)
    }
}

/// Anything that can produce an error code, required for use in a
/// [`GenericDiagnostic`]
pub trait ErrorCode {
    /// Get the error code associated with this instance
    fn error_code(&self) -> &'static str;
}

/// A diagnostic message produced by one of the zrc tools
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct GenericDiagnostic<K, LK, NK, HK>
where
    K: Debug + PartialEq + Eq + Display + ErrorCode + Serialize,
    LK: Debug + PartialEq + Eq + Display + Serialize,
    NK: Debug + PartialEq + Eq + Display + Serialize,
    HK: Debug + PartialEq + Eq + Display + Serialize,
{
    /// The severity of this diagnostic
    pub severity: Severity,
    /// The span and kind of this diagnostic's main message
    pub kind: Spanned<K>,

    /// A list of labels associated with this diagnostic
    pub labels: Vec<GenericLabel<LK>>,

    /// A list of notes (additional information) associated with this
    /// diagnostic, lacking spans
    pub notes: Vec<NK>,

    /// A list of help messages associated with this diagnostic, lacking spans
    ///
    /// These are typically suggestions for fixing the issue.
    pub helps: Vec<HK>,
}
impl<K, LK, NK, HK> Display for GenericDiagnostic<K, LK, NK, HK>
where
    K: Debug + PartialEq + Eq + Display + ErrorCode + Serialize,
    LK: Debug + PartialEq + Eq + Display + Serialize,
    HK: Debug + PartialEq + Eq + Display + Serialize,
    NK: Debug + PartialEq + Eq + Display + Serialize,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}[{}]: {}",
            self.severity,
            self.kind.value().error_code(),
            self.kind
        )
    }
}

impl<K, LK, NK, HK> GenericDiagnostic<K, LK, NK, HK>
where
    K: Debug + PartialEq + Eq + Display + ErrorCode + Serialize,
    LK: Debug + PartialEq + Eq + Display + Serialize,
    NK: Debug + PartialEq + Eq + Display + Serialize,
    HK: Debug + PartialEq + Eq + Display + Serialize,
{
    /// Convert this [`Diagnostic`] to a printable string using ariadne. The
    /// source code is provided directly as a string if it is not read from a
    /// file.
    ///
    /// # Panics
    /// This function may panic if the span is invalid or if writing to the
    /// buffer fails.
    #[must_use]
    pub fn print(&self, piped_source: Option<&str>) -> String {
        let span = self.kind.span();
        let message = self.kind.to_string();

        // read the source from the path inside of the span. if it is <stdin>, use
        // the provided piped source.
        let (source, path) = match span.file_name() {
            "/dev/<stdin>" => (
                piped_source
                    .expect("piped source must be provided for <stdin>")
                    .to_string(),
                "<stdin>",
            ),
            "<unknown>" => (
                piped_source
                    .expect("piped source must be provided for <unknown>")
                    .to_string(),
                "<unknown>",
            ),
            path => {
                let source = std::fs::read_to_string(path)
                    .unwrap_or_else(|_| panic!("failed to read source file {path} for diagnostic"));
                (source, path)
            }
        };

        // Create ariadne report using (filename, range) as the span type
        let mut report = Report::build(
            self.severity.to_report_kind(),
            (path, span.start()..span.end()),
        )
        .with_code(self.kind.value().error_code())
        .with_message(message)
        .with_labels(self.labels.iter().map(|label| {
            Label::new((path, label.kind.span().start()..label.kind.span().end()))
                .with_message(label.kind.to_string())
                .with_color(label.severity.color())
        }));

        report.with_notes(
            self.notes
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
        );
        report.with_helps(
            self.helps
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
        );

        let report = report.finish();

        // Write report to string
        let mut buffer = Vec::new();
        report
            .write((path, ariadne::Source::from(source)), &mut buffer)
            .expect("failed to write diagnostic");

        String::from_utf8(buffer).expect("diagnostic output should be valid UTF-8")
    }

    /// Convert this [`Diagnostic`] to JSON format using [`serde_json`].
    ///
    /// # Panics
    /// This function may panic if serialization fails (which should not happen
    /// in practice).
    #[must_use]
    pub fn print_json(&self) -> String {
        serde_json::to_string(self).expect("diagnostic serialization should succeed")
    }

    /// Emit this diagnostic in the specified format.
    ///
    /// This is a convenience method that calls either [`Self::print`] or
    /// [`Self::print_json`] based on the format.
    ///
    /// # Panics
    /// See [`Self::print`] and [`Self::print_json`] for panic conditions.
    #[must_use]
    pub fn emit(&self, format: DiagnosticEmitFormat, piped_source: Option<&str>) -> String {
        match format {
            DiagnosticEmitFormat::Human => self.print(piped_source),
            DiagnosticEmitFormat::Json => self.print_json(),
        }
    }

    /// Create a new empty diagnostic with the given severity and kind.
    const fn new_empty(severity: Severity, kind: Spanned<K>) -> Self {
        Self {
            severity,
            kind,
            labels: Vec::new(),
            notes: Vec::new(),
            helps: Vec::new(),
        }
    }

    /// Create an error.
    pub const fn error(kind: Spanned<K>) -> Self {
        Self::new_empty(Severity::Error, kind)
    }

    /// Create a warning.
    pub const fn warning(kind: Spanned<K>) -> Self {
        Self::new_empty(Severity::Warning, kind)
    }

    /// Add a new label to this diagnostic.
    #[must_use]
    pub fn with_label(mut self, label: GenericLabel<LK>) -> Self {
        self.labels.push(label);
        self
    }

    /// Add a new note to this diagnostic.
    #[must_use]
    pub fn with_note(mut self, note: NK) -> Self {
        self.notes.push(note);
        self
    }

    /// Add a new help message to this diagnostic.
    #[must_use]
    pub fn with_help(mut self, help: HK) -> Self {
        self.helps.push(help);
        self
    }
}
impl<K, LK, NK, HK> Error for GenericDiagnostic<K, LK, NK, HK>
where
    K: Debug + PartialEq + Eq + Display + ErrorCode + Serialize,
    LK: Debug + PartialEq + Eq + Display + Serialize,
    NK: Debug + PartialEq + Eq + Display + Serialize,
    HK: Debug + PartialEq + Eq + Display + Serialize,
{
}

/// A diagnostic message produced by the Zirco compiler
pub type Diagnostic = GenericDiagnostic<DiagnosticKind, LabelKind, NoteKind, HelpKind>;

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
        let diagnostic = Diagnostic::error(spanned_test!(0, DiagnosticKind::InvalidToken, 4));
        let display = diagnostic.to_string();
        assert!(display.contains("error"));
        assert!(display.contains("invalid token"));
    }

    #[test]
    #[ignore = "ignored until #603 complete"]
    fn diagnostic_print_formats_correctly() {
        let source = "let x = 5;";
        let diagnostic = Diagnostic::error(spanned!(4, DiagnosticKind::InvalidToken, 5, "<stdin>"));
        let output = diagnostic.print(Some(source));

        assert!(output.contains("<stdin>"));
        assert!(output.contains("Error"));
        assert!(output.contains("invalid token"));
    }

    #[test]
    fn diagnostic_print_json_produces_valid_json() {
        let diagnostic = Diagnostic::error(spanned_test!(0, DiagnosticKind::InvalidToken, 4));
        let json_output = diagnostic.print_json();

        // Verify it's valid JSON by parsing it
        let parsed: serde_json::Value =
            serde_json::from_str(&json_output).expect("should be valid JSON");

        // Check that key fields exist
        assert!(parsed.get("severity").is_some());
        assert!(parsed.get("kind").is_some());
    }

    #[test]
    #[ignore = "requires file access"]
    fn diagnostic_emit_human_format_calls_print() {
        let source = "let x = 5;";
        let diagnostic = Diagnostic::error(spanned!(4, DiagnosticKind::InvalidToken, 5, "<test>"));
        let output = diagnostic.emit(DiagnosticEmitFormat::Human, Some(source));

        // Should contain human-readable formatting
        assert!(output.contains("Error"));
    }

    #[test]
    fn diagnostic_emit_json_format_produces_json() {
        let diagnostic = Diagnostic::error(spanned_test!(0, DiagnosticKind::InvalidToken, 4));
        let json_output = diagnostic.emit(DiagnosticEmitFormat::Json, None);

        // Verify it's valid JSON
        assert!(serde_json::from_str::<serde_json::Value>(&json_output).is_ok());
    }

    #[test]
    fn diagnostic_with_labels_serializes_correctly() {
        let diagnostic =
            Diagnostic::error(spanned_test!(0, DiagnosticKind::InvalidToken, 4)).with_label(
                GenericLabel::error(spanned_test!(0, LabelKind::InvalidToken, 4)),
            );

        let json_output = diagnostic.print_json();
        let parsed: serde_json::Value =
            serde_json::from_str(&json_output).expect("should be valid JSON");

        // Check that labels array exists and has content
        let labels = parsed.get("labels").expect("should have labels field");
        assert!(labels.is_array());
        assert!(!labels.as_array().expect("should be array").is_empty());
    }

    #[test]
    fn diagnostic_with_notes_and_helps_serializes_correctly() {
        let diagnostic = Diagnostic::error(spanned_test!(0, DiagnosticKind::InvalidToken, 4))
            .with_note(NoteKind::ExpectedOneOfTokens(vec![
                "token1".to_string(),
                "token2".to_string(),
            ]))
            .with_help(HelpKind::JavascriptUserDetected("=="));

        let json_output = diagnostic.print_json();
        let parsed: serde_json::Value =
            serde_json::from_str(&json_output).expect("should be valid JSON");

        // Check that notes and helps arrays exist
        assert!(parsed.get("notes").is_some());
        assert!(parsed.get("helps").is_some());
    }
}
