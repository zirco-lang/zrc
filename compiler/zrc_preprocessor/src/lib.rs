#![doc = r"Preprocessor for the Zirco programming language"]
#![allow(unknown_lints)] // in case you use non-nightly clippy
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs,
    clippy::absolute_paths,
    clippy::as_conversions,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::deref_by_slicing,
    clippy::disallowed_script_idents,
    clippy::else_if_without_else,
    clippy::empty_structs_with_brackets,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::let_underscore_must_use,
    clippy::min_ident_chars,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::non_ascii_literal,
    clippy::redundant_type_annotations,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::unseparated_literal_suffix,
    clippy::implicit_clone,
    clippy::todo,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unneeded_field_pattern,
    clippy::wildcard_enum_match_arm,
    let_underscore_drop,
    macro_use_extern_crate,
    missing_debug_implementations,
    non_exhaustive_omitted_patterns,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    variant_size_differences,
    unused_qualifications,
    clippy::unwrap_used,
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    clippy::module_name_repetitions,
    clippy::doc_comment_double_space_linebreaks
)]

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use zrc_diagnostics::DiagnosticKind;
use zrc_utils::span::Span;

/// Preprocess a Zirco source file, handling `#include` directives.
///
/// This function processes the input source code and resolves all `#include` directives,
/// replacing them with the contents of the included files. It prevents circular includes
/// and handles both relative and absolute paths.
///
/// # Arguments
///
/// * `content` - The source code to preprocess
/// * `file_path` - The path to the file being processed (used for relative path resolution)
///
/// # Returns
///
/// Returns the preprocessed source code with all includes resolved, or a diagnostic error
/// if preprocessing fails.
///
/// # Errors
///
/// Returns a diagnostic error if:
/// * A file cannot be read
/// * A circular include is detected
/// * An include path is invalid
pub fn preprocess(
    content: &str,
    file_path: &Path,
) -> Result<String, zrc_diagnostics::Diagnostic> {
    let mut processed_files = HashSet::new();
    preprocess_recursive(content, file_path, &mut processed_files)
}

/// Recursively preprocess a file, tracking already-included files to prevent cycles.
fn preprocess_recursive(
    content: &str,
    file_path: &Path,
    processed_files: &mut HashSet<PathBuf>,
) -> Result<String, zrc_diagnostics::Diagnostic> {
    // Canonicalize the file path to track it properly
    let canonical_path = file_path
        .canonicalize()
        .map_err(|err| {
            DiagnosticKind::PreprocessorError(format!(
                "failed to canonicalize path {}: {}",
                file_path.display(),
                err
            ))
            .error_in(Span::from_positions(0, 0))
        })?;

    // Check for circular includes
    if processed_files.contains(&canonical_path) {
        return Err(DiagnosticKind::PreprocessorCircularInclude(
            file_path.display().to_string(),
        )
        .error_in(Span::from_positions(0, 0)));
    }

    processed_files.insert(canonical_path);

    let mut result = String::new();
    let parent_dir = file_path.parent().ok_or_else(|| {
        DiagnosticKind::PreprocessorError(format!(
            "failed to get parent directory of {}",
            file_path.display()
        ))
        .error_in(Span::from_positions(0, 0))
    })?;

    for line in content.lines() {
        let trimmed = line.trim();

        // Check if this is an include directive
        if let Some(include_path_str) = trimmed.strip_prefix("#include") {
            let include_path_str = include_path_str.trim();

            // Parse the include path (support both "file" and <file> syntax)
            let include_path = if let Some(quoted) = include_path_str
                .strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
            {
                quoted
            } else if let Some(angled) = include_path_str
                .strip_prefix('<')
                .and_then(|s| s.strip_suffix('>'))
            {
                angled
            } else {
                return Err(DiagnosticKind::PreprocessorInvalidIncludeSyntax(
                    include_path_str.to_string(),
                )
                .error_in(Span::from_positions(0, 0)));
            };

            // Resolve the include path relative to the current file
            let resolved_path = parent_dir.join(include_path);

            // Read the included file
            let included_content = std::fs::read_to_string(&resolved_path).map_err(|err| {
                DiagnosticKind::PreprocessorFileNotFound {
                    path: resolved_path.display().to_string(),
                    error: err.to_string(),
                }
                .error_in(Span::from_positions(0, 0))
            })?;

            // Recursively preprocess the included file
            let processed_included =
                preprocess_recursive(&included_content, &resolved_path, processed_files)?;

            result.push_str(&processed_included);
            result.push('\n');
        } else {
            // Not an include directive, keep the line as-is
            result.push_str(line);
            result.push('\n');
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn preprocess_without_includes_returns_same_content() {
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_no_include.zr");
        
        let content = "fn main() {\n    return 0;\n}";
        fs::write(&test_file, content).expect("Failed to write test file");
        
        let result = preprocess(content, &test_file).expect("Preprocessing should succeed");
        
        // The result should have content plus newlines
        assert!(result.contains("fn main()"));
        assert!(result.contains("return 0;"));
        
        // Cleanup
        drop(fs::remove_file(&test_file));
    }

    #[test]
    fn preprocess_with_include_combines_files() {
        let temp_dir = std::env::temp_dir();
        let included_file = temp_dir.join("included.zr");
        let main_file = temp_dir.join("main.zr");
        
        // Create included file
        let included_content = "fn helper() {\n    return 1;\n}";
        fs::write(&included_file, included_content).expect("Failed to write included file");
        
        // Create main file with include
        let main_content = "#include \"included.zr\"\nfn main() {\n    return helper();\n}";
        fs::write(&main_file, main_content).expect("Failed to write main file");
        
        let result = preprocess(main_content, &main_file).expect("Preprocessing should succeed");
        
        // The result should contain both files' content
        assert!(result.contains("fn helper()"));
        assert!(result.contains("fn main()"));
        assert!(result.contains("return 1;"));
        assert!(result.contains("return helper();"));
        
        // Cleanup
        drop(fs::remove_file(&included_file));
        drop(fs::remove_file(&main_file));
    }

    #[test]
    fn preprocess_detects_circular_includes() {
        let temp_dir = std::env::temp_dir();
        let file_a = temp_dir.join("circular_a.zr");
        let file_b = temp_dir.join("circular_b.zr");
        
        // Create file A that includes B
        fs::write(&file_a, "#include \"circular_b.zr\"").expect("Failed to write file A");
        
        // Create file B that includes A (circular)
        fs::write(&file_b, "#include \"circular_a.zr\"").expect("Failed to write file B");
        
        let result = preprocess("#include \"circular_b.zr\"", &file_a);
        
        // Should error due to circular include
        assert!(result.is_err());
        if let Err(err) = result {
            assert!(err.to_string().contains("circular"));
        }
        
        // Cleanup
        drop(fs::remove_file(&file_a));
        drop(fs::remove_file(&file_b));
    }

    #[test]
    fn preprocess_errors_on_missing_file() {
        let temp_dir = std::env::temp_dir();
        let main_file = temp_dir.join("missing_include.zr");
        
        let content = "#include \"nonexistent.zr\"";
        fs::write(&main_file, content).expect("Failed to write main file");
        
        let result = preprocess(content, &main_file);
        
        // Should error because included file doesn't exist
        assert!(result.is_err());
        
        // Cleanup
        drop(fs::remove_file(&main_file));
    }

    #[test]
    fn preprocess_supports_angle_bracket_syntax() {
        let temp_dir = std::env::temp_dir();
        let included_file = temp_dir.join("lib.zr");
        let main_file = temp_dir.join("angle_main.zr");
        
        // Create included file
        fs::write(&included_file, "fn lib_func() {}").expect("Failed to write included file");
        
        // Create main file with angle bracket include
        let main_content = "#include <lib.zr>\nfn main() {}";
        fs::write(&main_file, main_content).expect("Failed to write main file");
        
        let result = preprocess(main_content, &main_file).expect("Preprocessing should succeed");
        
        // The result should contain both files' content
        assert!(result.contains("fn lib_func()"));
        assert!(result.contains("fn main()"));
        
        // Cleanup
        drop(fs::remove_file(&included_file));
        drop(fs::remove_file(&main_file));
    }
}
