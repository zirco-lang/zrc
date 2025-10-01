//! Diagnostic formatting using `codespan-reporting`
//!
//! This module provides an alternative diagnostic formatter using the
//! `codespan-reporting` crate, which offers rich, customizable error messages
//! with support for multiple code spans, labels, and notes.

use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label, Severity as CodespanSeverity};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::Buffer;

use crate::{Diagnostic, Severity};

impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string using codespan-reporting
    #[must_use]
    pub fn print_with_codespan(&self, source: &str, file_name: &str) -> String {
        // Create a simple file database with just one file
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, source);

        // Convert our diagnostic to codespan format
        let severity = match self.0 {
            Severity::Error => CodespanSeverity::Error,
        };

        let diagnostic = CodespanDiagnostic::new(severity)
            .with_message(self.1.to_string())
            .with_labels(vec![Label::primary(file_id, self.1.span().start()..self.1.span().end())]);

        // Render to a buffer
        let mut buffer = Buffer::ansi();
        let config = term::Config::default();
        
        term::emit(&mut buffer, &config, &files, &diagnostic)
            .expect("failed to emit diagnostic");

        String::from_utf8_lossy(buffer.as_slice()).to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::{DiagnosticKind, SpanExt};
    use zrc_utils::span::Span;

    #[test]
    fn test_codespan_formatter() {
        let source = "let x = 5;\nlet y = unknown_var;";
        let span = Span::from_positions(20, 31);
        
        let diagnostic = span.error(DiagnosticKind::UnableToResolveIdentifier("unknown_var".to_string()));
        
        let output = diagnostic.print_with_codespan(source, "test.zr");
        
        // The output should contain the error message and source location
        assert!(output.contains("unable to resolve identifier"));
        assert!(output.contains("unknown_var"));
    }
}
