//! Diagnostic collection for error recovery during type checking

use std::cell::RefCell;

use zrc_diagnostics::Diagnostic;

/// A collector for diagnostics during type checking that allows error recovery.
///
/// This structure uses interior mutability to allow collecting diagnostics
/// while maintaining immutable references to the type checker state.
#[derive(Debug, Default)]
pub struct DiagnosticCollector {
    /// The collected diagnostics
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl DiagnosticCollector {
    /// Create a new empty diagnostic collector
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    /// Add a diagnostic to the collection
    pub fn push(&self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }

    /// Get all collected diagnostics, consuming the collector
    #[must_use]
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }

    /// Check if any diagnostics have been collected
    #[must_use]
    pub fn has_errors(&self) -> bool {
        !self.diagnostics.borrow().is_empty()
    }

    /// Get the number of diagnostics collected
    #[must_use]
    pub fn len(&self) -> usize {
        self.diagnostics.borrow().len()
    }

    /// Check if the collector is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.diagnostics.borrow().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use zrc_diagnostics::{DiagnosticKind, Severity};
    use zrc_utils::spanned_test;

    use super::*;

    #[test]
    fn collector_starts_empty() {
        let collector = DiagnosticCollector::new();
        assert!(collector.is_empty());
        assert_eq!(collector.len(), 0);
        assert!(!collector.has_errors());
    }

    #[test]
    fn collector_can_collect_diagnostics() {
        let collector = DiagnosticCollector::new();
        
        collector.push(Diagnostic(
            Severity::Error,
            spanned_test!(0, DiagnosticKind::InvalidToken, 5),
        ));
        
        assert!(!collector.is_empty());
        assert_eq!(collector.len(), 1);
        assert!(collector.has_errors());
    }

    #[test]
    fn collector_can_collect_multiple_diagnostics() {
        let collector = DiagnosticCollector::new();
        
        collector.push(Diagnostic(
            Severity::Error,
            spanned_test!(0, DiagnosticKind::InvalidToken, 5),
        ));
        
        collector.push(Diagnostic(
            Severity::Error,
            spanned_test!(10, DiagnosticKind::InvalidToken, 15),
        ));
        
        assert_eq!(collector.len(), 2);
        
        let diagnostics = collector.into_diagnostics();
        assert_eq!(diagnostics.len(), 2);
    }
}
