//! Defines types for Zirco compile time diagnostics.

use std::error::Error;

use ansi_term::{Color, Style};
use derive_more::Display;
use zrc_utils::span::Spanned;

use crate::{DiagnosticKind, fmt::display_source_window};

/// The severity of a [`Diagnostic`].
#[derive(Clone, PartialEq, Eq, Debug, Display)]
#[display("{}", self.style().paint(self.text()))]
pub enum Severity {
    /// Error. Compilation will not continue.
    Error,
}
impl Severity {
    /// Get the display [`Style`] of this severity
    pub(crate) fn style(&self) -> Style {
        match *self {
            Self::Error => Color::Red.bold(),
        }
    }

    /// Get this severity's name
    pub(crate) const fn text(&self) -> &'static str {
        match *self {
            Self::Error => "error",
        }
    }
}

/// A diagnostic message produced by zrc
#[derive(Debug, PartialEq, Eq, Display)]
#[display("{_0}: {_1}")]
pub struct Diagnostic(pub Severity, pub Spanned<DiagnosticKind>);
impl Diagnostic {
    /// Convert this [`Diagnostic`] to a printable string
    #[must_use]
    pub fn print(&self, source: &str) -> String {
        format!(
            "{}: {}\n{}",
            self.0,
            Color::White.bold().paint(self.1.to_string()),
            display_source_window(&self.0, self.1.span(), source)
        )
    }
}
impl Error for Diagnostic {}
