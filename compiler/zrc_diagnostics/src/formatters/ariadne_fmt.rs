//! Diagnostic formatting using `ariadne`
//!
//! This module provides an alternative diagnostic formatter using the
//! `ariadne` crate, which is known for its beautiful, colorful error messages
//! inspired by Rust's own error formatting.

use ariadne::{Color as AriadneColor, Label, Report, ReportKind, Source};

use crate::{Diagnostic, Severity};

impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string using ariadne
    #[must_use]
    pub fn print_with_ariadne(&self, source: &str, file_name: &str) -> String {
        // Convert our diagnostic to ariadne format
        let kind = match self.0 {
            Severity::Error => ReportKind::Error,
        };

        let report = Report::build(kind, file_name, self.1.span().start())
            .with_message(self.1.to_string())
            .with_label(
                Label::new((file_name, self.1.span().start()..self.1.span().end()))
                    .with_color(AriadneColor::Red)
            )
            .finish();

        // Render to a string buffer
        let mut buffer = Vec::new();
        report
            .write((file_name, Source::from(source)), &mut buffer)
            .expect("failed to write diagnostic");

        String::from_utf8_lossy(&buffer).to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::{DiagnosticKind, SpanExt};
    use zrc_utils::span::Span;

    #[test]
    fn test_ariadne_formatter() {
        let source = "let x = 5;\nlet y = unknown_var;";
        let span = Span::from_positions(20, 31);
        
        let diagnostic = span.error(DiagnosticKind::UnableToResolveIdentifier("unknown_var".to_string()));
        
        let output = diagnostic.print_with_ariadne(source, "test.zr");
        
        // The output should contain the error message and source location
        assert!(output.contains("unable to resolve identifier"));
        assert!(output.contains("unknown_var"));
    }
}
