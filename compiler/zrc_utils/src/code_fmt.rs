//! Utilities for formatting code in outputs

/// Helper function to indent all lines of a string by a given prefix
#[must_use]
pub fn indent_lines(code: &str, prefix: &str) -> String {
    code.lines()
        .map(|line| format!("{prefix}{line}"))
        .collect::<Vec<_>>()
        .join("\n")
}
