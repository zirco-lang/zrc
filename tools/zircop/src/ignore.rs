//! Utilities for parsing zircop ignore directives from source code comments

use std::collections::HashMap;

/// A parsed ignore directive from a source comment
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IgnoreDirective {
    /// Ignore a specific lint on the current line
    /// Format: `// zircop-ignore: lint_name`
    Current {
        /// The line number (1-indexed)
        line: usize,
        /// The lint name to ignore
        lint_name: String,
    },
    /// Ignore a specific lint on the next line
    /// Format: `// zircop-ignore-next-line: lint_name`
    NextLine {
        /// The line number (1-indexed) where the comment appears
        line: usize,
        /// The lint name to ignore
        lint_name: String,
    },
}

/// Parse ignore directives from source code comments
///
/// This function scans the source code for special comments that start with
/// `zircop-ignore:` or `zircop-ignore-next-line:` and returns a map of
/// line numbers to lint names that should be ignored on those lines.
#[must_use]
pub fn parse_ignore_directives(source: &str) -> HashMap<usize, Vec<String>> {
    let mut ignores: HashMap<usize, Vec<String>> = HashMap::new();

    for (line_idx, line) in source.lines().enumerate() {
        let line_number = line_idx + 1; // 1-indexed

        // Look for single-line comments
        if let Some(comment_start) = line.find("//") {
            let comment = &line[comment_start + 2..].trim();

            // Check for zircop-ignore-next-line
            if let Some(rest) = comment.strip_prefix("zircop-ignore-next-line:") {
                let lint_name = rest.trim().to_string();
                if !lint_name.is_empty() {
                    ignores.entry(line_number + 1).or_default().push(lint_name);
                }
            } else if let Some(rest) = comment.strip_prefix("zircop-ignore:") {
                // Check for zircop-ignore (applies to current line)
                let lint_name = rest.trim().to_string();
                if !lint_name.is_empty() {
                    ignores.entry(line_number).or_default().push(lint_name);
                }
            } else {
                // Not a zircop directive, ignore
            }
        }
    }

    ignores
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ignore_current_line() {
        let source = r"
fn f() -> i32 {
    let unused = 10; // zircop-ignore: unused_variable
    return 0;
}
";
        let ignores = parse_ignore_directives(source);
        assert_eq!(ignores.get(&3), Some(&vec!["unused_variable".to_string()]));
    }

    #[test]
    fn test_parse_ignore_next_line() {
        let source = r"
fn f() -> i32 {
    // zircop-ignore-next-line: unused_variable
    let unused = 10;
    return 0;
}
";
        let ignores = parse_ignore_directives(source);
        assert_eq!(ignores.get(&4), Some(&vec!["unused_variable".to_string()]));
    }

    #[test]
    fn test_parse_multiple_ignores() {
        let source = r"
fn f() -> i32 {
    // zircop-ignore-next-line: unused_variable
    let unused = 10;
    let _var = 7; // zircop-ignore: underscore_variable_used
    return 0;
}
";
        let ignores = parse_ignore_directives(source);
        assert_eq!(ignores.get(&4), Some(&vec!["unused_variable".to_string()]));
        assert_eq!(
            ignores.get(&5),
            Some(&vec!["underscore_variable_used".to_string()])
        );
    }

    #[test]
    fn test_no_ignores() {
        let source = r"
fn f() -> i32 {
    let unused = 10;
    return 0;
}
";
        let ignores = parse_ignore_directives(source);
        assert!(ignores.is_empty());
    }
}
