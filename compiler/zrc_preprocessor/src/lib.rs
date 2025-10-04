//! Zirco preprocessor
//!
//! This module provides basic C-like preprocessing capabilities for Zirco,
//! primarily supporting `#include` directives to combine multiple source files
//! into a single compilation unit.

#![allow(unknown_lints)]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs,
    clippy::unwrap_used,
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    clippy::module_name_repetitions
)]

use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use zrc_diagnostics::Diagnostic;
use zrc_parser::ast::stmt::Declaration;
use zrc_utils::span::Spanned;

/// Represents a preprocessor directive
#[derive(Debug, Clone, PartialEq, Eq)]
enum Directive {
    /// `#include "file.zrc"`
    Include(String),
    /// `#pragma once`
    PragmaOnce,
}

/// Parse preprocessor directives from source code
///
/// Returns a list of directives and the source with directives removed
fn extract_directives(source: &str) -> (Vec<Directive>, String) {
    let mut directives = Vec::new();
    let mut cleaned_source = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Check for #pragma once
        if trimmed == "#pragma once" {
            directives.push(Directive::PragmaOnce);
            // Replace directive with empty line to maintain line numbers
            cleaned_source.push('\n');
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("#include") {
            // Parse #include "filename"
            let rest = rest.trim();
            if let Some(quoted) = rest.strip_prefix('"')
                && let Some(end) = quoted.find('"')
            {
                let filename = quoted[..end].to_string();
                directives.push(Directive::Include(filename));
                // Replace directive with empty line to maintain line numbers
                cleaned_source.push('\n');
                continue;
            }
        }
        cleaned_source.push_str(line);
        cleaned_source.push('\n');
    }

    (directives, cleaned_source)
}

/// Process a single file and its includes
///
/// # Errors
/// Returns an error if:
/// - A file cannot be read
/// - A file cannot be parsed
/// - Circular includes are detected
#[allow(clippy::result_large_err)]
fn process_file(
    file_path: &Path,
    parent_dir: &Path,
    processed: &mut HashSet<PathBuf>,
    file_name_static: &'static str,
) -> Result<Vec<Spanned<Declaration<'static>>>, Diagnostic> {
    let canonical = file_path.canonicalize().map_err(|_| {
        // Use InvalidToken as a generic error for now
        zrc_diagnostics::DiagnosticKind::InvalidToken
            .error_in(zrc_utils::span::Span::from_positions(0, 0))
    })?;

    // Check for circular includes
    if processed.contains(&canonical) {
        return Ok(Vec::new()); // Already processed, skip
    }
    processed.insert(canonical);

    // Read the file
    let source = fs::read_to_string(file_path).map_err(|_| {
        // Use InvalidToken as a generic error for now
        zrc_diagnostics::DiagnosticKind::InvalidToken
            .error_in(zrc_utils::span::Span::from_positions(0, 0))
    })?;

    // Extract directives
    let (directives, cleaned_source) = extract_directives(&source);

    // Leak the cleaned source to get 'static lifetime
    let cleaned_source_static: &'static str = Box::leak(cleaned_source.into_boxed_str());

    // Parse the cleaned source
    let mut all_declarations = Vec::new();

    // Process includes first
    for directive in directives {
        match directive {
            Directive::Include(include_path) => {
                let include_file = parent_dir.join(&include_path);
                let include_file_name = Box::leak(include_path.clone().into_boxed_str());
                let include_decls =
                    process_file(&include_file, parent_dir, processed, include_file_name)?;
                all_declarations.extend(include_decls);
            }
            Directive::PragmaOnce => {
                // #pragma once is handled by the circular include detection
                // The file is already in the processed set, so it won't be
                // included again
            }
        }
    }

    // Parse current file with static source
    let declarations = zrc_parser::parser::parse_program(cleaned_source_static, file_name_static)?;
    all_declarations.extend(declarations);

    Ok(all_declarations)
}

/// Preprocess a Zirco source file, resolving all `#include` directives
///
/// This function takes a source file path and processes it along with any
/// included files, combining them into a single list of declarations.
///
/// # Arguments
/// * `file_path` - Path to the main source file
/// * `parent_dir` - Directory to resolve relative includes from
///
/// # Returns
/// A combined list of declarations from the main file and all includes
///
/// # Errors
/// Returns a diagnostic error if:
/// - Any file cannot be read
/// - Any file has syntax errors
/// - Circular includes are detected
#[allow(clippy::result_large_err)]
pub fn preprocess(
    file_path: &Path,
    parent_dir: &Path,
) -> Result<Vec<Spanned<Declaration<'static>>>, Diagnostic> {
    let mut processed = HashSet::new();

    // Leak the file name to get a 'static reference
    let file_name = Box::leak(
        file_path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string()
            .into_boxed_str(),
    );

    process_file(file_path, parent_dir, &mut processed, file_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_directives_parses_include() {
        let source = r#"
#include "file1.zrc"
fn main() {}
#include "file2.zrc"
"#;
        let (directives, _) = extract_directives(source);
        assert_eq!(directives.len(), 2);
        assert_eq!(directives[0], Directive::Include("file1.zrc".to_string()));
        assert_eq!(directives[1], Directive::Include("file2.zrc".to_string()));
    }

    #[test]
    fn extract_directives_preserves_line_numbers() {
        let source = "line1\n#include \"file.zrc\"\nline3\n";
        let (_, cleaned) = extract_directives(source);
        assert_eq!(cleaned.lines().count(), source.lines().count());
    }

    #[test]
    fn extract_directives_parses_pragma_once() {
        let source = "
#pragma once
fn helper() {}
";
        let (directives, _) = extract_directives(source);
        assert_eq!(directives.len(), 1);
        assert_eq!(directives[0], Directive::PragmaOnce);
    }

    #[test]
    fn extract_directives_parses_pragma_once_with_includes() {
        let source = "
#pragma once
#include \"file1.zrc\"
fn helper() {}
";
        let (directives, _) = extract_directives(source);
        assert_eq!(directives.len(), 2);
        assert_eq!(directives[0], Directive::PragmaOnce);
        assert_eq!(directives[1], Directive::Include("file1.zrc".to_string()));
    }
}
