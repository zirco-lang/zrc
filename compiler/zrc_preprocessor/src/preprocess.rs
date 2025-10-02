//! Preprocessor for Zirco source files
//!
//! This module implements a simple file inclusion system via `#include
//! "file.zr"` directives. Unlike traditional C preprocessors that work at the
//! text level, this preprocessor works at the AST level - each file is parsed
//! separately, and the ASTs are combined. This preserves source location
//! information for better error messages.

use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_utils::span::Span;

/// Information about a source file's content
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// Path to the source file
    pub path: String,
    /// Content of the source file
    pub content: String,
}

/// Result of preprocessing: a main file plus all included files
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessedFiles {
    /// The main source file
    pub main_file: SourceFile,
    /// All included files in the order they should be processed
    pub included_files: Vec<SourceFile>,
}

/// Preprocess a Zirco source file, collecting all `#include` directives
///
/// This function reads the main file and recursively finds all included files.
/// It returns all the source files that need to be compiled, in dependency
/// order.
///
/// # Arguments
/// * `file_path` - Path to the file to preprocess
/// * `content` - Content of the file
///
/// # Errors
/// Returns an error if:
/// - An include directive is malformed
/// - An included file cannot be read
/// - A circular include is detected
///
/// # Example
/// ```
/// use zrc_preprocessor::preprocess::preprocess;
///
/// let content = r#"fn main() { return 0; }"#;
/// let result = preprocess("main.zr", content);
/// assert!(result.is_ok());
/// ```
pub fn preprocess(file_path: &str, content: &str) -> Result<PreprocessedFiles, Diagnostic> {
    let mut preprocessor = Preprocessor::new();
    preprocessor.collect_files(file_path, content)
}

/// Internal preprocessor state
struct Preprocessor {
    /// Set of files already included to detect circular includes
    included_files: HashSet<PathBuf>,
    /// List of collected source files
    files: Vec<SourceFile>,
    /// Set of files that have #pragma once
    pragma_once_files: HashSet<PathBuf>,
}

impl Preprocessor {
    /// Create a new preprocessor
    fn new() -> Self {
        Self {
            included_files: HashSet::new(),
            files: Vec::new(),
            pragma_once_files: HashSet::new(),
        }
    }

    /// Collect all source files recursively
    fn collect_files(
        &mut self,
        file_path: &str,
        content: &str,
    ) -> Result<PreprocessedFiles, Diagnostic> {
        // Process includes in the main file first
        self.process_file_recursive(file_path, content, 0)?;

        // Create a source file without includes for the main file content
        let content_without_includes = Self::remove_includes(content);

        let main_file = SourceFile {
            path: file_path.to_string(),
            content: content_without_includes,
        };

        Ok(PreprocessedFiles {
            main_file,
            included_files: self.files.clone(),
        })
    }

    /// Remove #include directives from content
    fn remove_includes(content: &str) -> String {
        content
            .lines()
            .filter(|line| {
                let trimmed = line.trim();
                !trimmed.starts_with("#include") && !trimmed.starts_with("#pragma once")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Process a file recursively, collecting includes
    fn process_file_recursive(
        &mut self,
        file_path: &str,
        content: &str,
        depth: usize,
    ) -> Result<(), Diagnostic> {
        // Prevent infinite recursion
        const MAX_INCLUDE_DEPTH: usize = 32;
        if depth > MAX_INCLUDE_DEPTH {
            return Err(DiagnosticKind::PreprocessorError(format!(
                "maximum include depth of {MAX_INCLUDE_DEPTH} exceeded"
            ))
            .error_in(Span::from_positions(0, 0)));
        }

        // Track this file as included
        let canonical_path = PathBuf::from(file_path);
        if !self.included_files.insert(canonical_path.clone()) {
            return Err(DiagnosticKind::PreprocessorError(format!(
                "circular include detected for file: {file_path}"
            ))
            .error_in(Span::from_positions(0, 0)));
        }

        // Process all includes in this file
        let mut current_pos = 0;
        for line in content.lines() {
            let line_start = current_pos;
            let trimmed = line.trim();

            if let Some(include_path) = trimmed.strip_prefix("#include") {
                // Parse the include directive
                let include_path = include_path.trim();

                // Extract the file path from quotes
                if !include_path.starts_with('"') || !include_path.ends_with('"') {
                    return Err(DiagnosticKind::PreprocessorError(
                        "include directive must be in format: #include \"file.zr\"".to_string(),
                    )
                    .error_in(Span::from_positions(line_start, line_start + line.len())));
                }

                let included_file = &include_path[1..include_path.len() - 1];

                // Resolve the path relative to the current file
                let resolved_path = Self::resolve_include_path(file_path, included_file);

                // Read the included file
                let included_content = fs::read_to_string(&resolved_path).map_err(|err| {
                    DiagnosticKind::PreprocessorError(format!(
                        "failed to read included file '{}': {}",
                        resolved_path.display(),
                        err
                    ))
                    .error_in(Span::from_positions(line_start, line_start + line.len()))
                })?;

                let resolved_path_buf = resolved_path;
                let resolved_path_str = resolved_path_buf.to_string_lossy().to_string();

                // Check if this file has #pragma once and was already processed
                let has_pragma_once = included_content
                    .lines()
                    .any(|line| line.trim() == "#pragma once");
                if has_pragma_once && self.pragma_once_files.contains(&resolved_path_buf) {
                    // File has #pragma once and was already included, skip it
                    current_pos = line_start + line.len() + 1;
                    continue;
                }

                // Mark file with #pragma once
                if has_pragma_once {
                    self.pragma_once_files.insert(resolved_path_buf.clone());
                }

                // Add this file to our collection (with preprocessor directives removed)
                self.files.push(SourceFile {
                    path: resolved_path_str.clone(),
                    content: Self::remove_includes(&included_content),
                });

                // Recursively process the included file
                self.process_file_recursive(&resolved_path_str, &included_content, depth + 1)?;
            }

            current_pos = line_start + line.len() + 1; // +1 for newline
        }

        // Remove this file from included set when done
        self.included_files.remove(&canonical_path);

        Ok(())
    }

    /// Resolve an include path relative to the current file
    fn resolve_include_path(current_file: &str, include_path: &str) -> PathBuf {
        // Get the directory of the current file
        let current_path = Path::new(current_file);
        let base_dir = current_path
            .parent()
            .map_or_else(|| Path::new("."), |parent| parent);

        // Resolve the include path relative to the current file's directory
        base_dir.join(include_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preprocess_without_includes_works() {
        let content = "fn main() {\n    return 0;\n}\n";
        let result = preprocess("test.zr", content);

        assert!(result.is_ok());
        let preprocessed = result.expect("preprocessing should succeed");
        assert_eq!(preprocessed.main_file.path, "test.zr");
        // Content might have different newline handling
        assert!(preprocessed.main_file.content.contains("fn main()"));
        assert!(preprocessed.included_files.is_empty());
    }
}
