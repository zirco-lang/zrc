//! Preprocessor for the Zirco programming language
//!
//! This module provides preprocessing capabilities including `#include`
//! directives and `#pragma once` support for the Zirco language.

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
    clippy::decimal_literal_replacement,
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

use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use zrc_diagnostics::{Diagnostic, DiagnosticKind, LabelKind, NoteKind, diagnostic::GenericLabel};
use zrc_utils::span::{Span, Spannable};

/// Represents a chunk of source code with its metadata
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceChunk {
    /// The file name where this chunk originates from
    pub file_name: String,
    /// The starting line number (1-indexed) in the original file
    pub start_line: usize,
    /// The byte offset in the original file where this chunk starts
    pub byte_offset: usize,
    /// The content of this chunk
    pub content: String,
}

impl SourceChunk {
    /// Create a new source chunk
    #[must_use]
    pub const fn new(
        file_name: String,
        start_line: usize,
        byte_offset: usize,
        content: String,
    ) -> Self {
        Self {
            file_name,
            start_line,
            byte_offset,
            content,
        }
    }
}

/// Context for preprocessing operations
#[derive(Debug)]
struct PreprocessorCtx {
    /// Set of files that have been included with `#pragma once`
    pragma_once_files: HashSet<PathBuf>,
    /// Collected source chunks
    chunks: Vec<SourceChunk>,
    /// The paths to search for bracket includes
    search_paths: Vec<&'static Path>,
}

impl PreprocessorCtx {
    /// Create a new preprocessor context
    fn new(search_paths: Vec<&'static Path>) -> Self {
        Self {
            pragma_once_files: HashSet::new(),
            chunks: Vec::new(),
            search_paths,
        }
    }
}

/// Search for an include file in the provided search paths
fn find_include_file(ctx: &PreprocessorCtx, include_file: &str) -> Option<PathBuf> {
    for search_path in &ctx.search_paths {
        let candidate = search_path.join(include_file);
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

/// Process a Zirco source file with preprocessing directives
///
/// # Arguments
/// * `base_path` - The directory to resolve relative includes from
/// * `file_name` - The name of the file being processed
/// * `content` - The content of the file to preprocess
///
/// # Errors
/// Returns an error if:
/// - An included file cannot be found
/// - An included file cannot be read
/// - A preprocessing directive is malformed
#[expect(clippy::result_large_err)]
pub fn preprocess(
    base_path: &Path,
    search_paths: Vec<&'static Path>,
    file_name: &str,
    content: &str,
) -> Result<Vec<SourceChunk>, Diagnostic> {
    let mut ctx = PreprocessorCtx::new(search_paths);
    preprocess_internal(base_path, file_name, content, &mut ctx)?;
    Ok(ctx.chunks)
}

/// Internal recursive preprocessing function
#[expect(clippy::too_many_lines, clippy::result_large_err)]
fn preprocess_internal(
    base_path: &Path,
    file_name: &str,
    content: &str,
    ctx: &mut PreprocessorCtx,
) -> Result<(), Diagnostic> {
    let mut current_chunk_lines = Vec::new();
    let mut chunk_start_line = 1;
    let mut chunk_start_byte = 0;
    let mut current_byte = 0;
    let mut has_pragma_once = false;

    // Create a static file name string for use in spans
    // Using Box::leak is intentional here as file names need to outlive the
    // compilation process
    let static_file_name: &'static str = Box::leak(
        base_path
            .join(file_name)
            .to_string_lossy()
            .into_owned()
            .into_boxed_str(),
    );

    for (line_num, line) in content.lines().enumerate() {
        let line_num = line_num + 1; // Convert to 1-indexed
        let trimmed = line.trim();

        if let Some(directive) = trimmed.strip_prefix('#') {
            // Strip any trailing comments from the directive
            let directive = directive.split("//").next().unwrap_or(directive).trim();

            // Support "pragma once" with any amount of whitespace
            if directive
                .strip_prefix("pragma")
                .is_some_and(|suffix| suffix.trim() == "once")
            {
                has_pragma_once = true;
                // Flush current chunk if it has content before the pragma directive
                if !current_chunk_lines.is_empty() {
                    ctx.chunks.push(SourceChunk::new(
                        static_file_name.to_string(),
                        chunk_start_line,
                        chunk_start_byte,
                        current_chunk_lines.join("\n"),
                    ));
                    current_chunk_lines.clear();
                }
                // Don't add this line to chunks, it's just a directive
                chunk_start_line = line_num + 1;
                // Update byte offset to skip this line
                chunk_start_byte = current_byte + line.len() + 1; // +1 for newline
            } else if let Some(include_path) = directive.strip_prefix("include") {
                // Flush current chunk if it has content
                if !current_chunk_lines.is_empty() {
                    ctx.chunks.push(SourceChunk::new(
                        static_file_name.to_string(),
                        chunk_start_line,
                        chunk_start_byte,
                        current_chunk_lines.join("\n"),
                    ));
                    current_chunk_lines.clear();
                }

                // Parse include path
                let include_path = include_path.trim();
                let (mut include_file, do_search_path) =
                    if let Some(path) = include_path.strip_prefix('"') {
                        (
                            path.strip_suffix('"')
                                .ok_or_else(|| {
                                    let sp = Span::from_positions_and_file(
                                        current_byte,
                                        current_byte + line.len(),
                                        static_file_name,
                                    );
                                    DiagnosticKind::PreprocessorUnterminatedIncludeDirective
                                        .error_in(sp)
                                        .with_label(GenericLabel::error(
                                            LabelKind::ExpectedClosing("\"".into()).in_span(sp),
                                        ))
                                })?
                                .to_string(),
                            false,
                        )
                    } else if let Some(path) = include_path.strip_prefix('<') {
                        (
                            path.strip_suffix('>')
                                .ok_or_else(|| {
                                    let sp = Span::from_positions_and_file(
                                        current_byte,
                                        current_byte + line.len(),
                                        static_file_name,
                                    );
                                    DiagnosticKind::PreprocessorUnterminatedIncludeDirective
                                        .error_in(sp)
                                        .with_label(GenericLabel::error(
                                            LabelKind::ExpectedClosing(">".into()).in_span(sp),
                                        ))
                                })?
                                .to_string(),
                            true,
                        )
                    } else {
                        let sp = Span::from_positions_and_file(
                            current_byte,
                            current_byte + line.len(),
                            static_file_name,
                        );
                        return Err(DiagnosticKind::PreprocessorInvalidIncludeSyntax
                            .error_in(sp)
                            .with_label(GenericLabel::error(
                                LabelKind::PreprocessorInvalidIncludeSyntax.in_span(sp),
                            ))
                            .with_note(NoteKind::ValidIncludeSyntax));
                    };

                // Resolve the include file path

                if do_search_path {
                    include_file = find_include_file(ctx, &include_file)
                        .ok_or_else(|| {
                            let sp = Span::from_positions_and_file(
                                current_byte,
                                current_byte + line.len(),
                                static_file_name,
                            );
                            DiagnosticKind::PreprocessorCannotFindIncludeFile
                                .error_in(sp)
                                .with_label(GenericLabel::error(
                                    LabelKind::PreprocessorCannotFindIncludeFile(
                                        include_file.clone(),
                                    )
                                    .in_span(sp),
                                ))
                                .with_note(NoteKind::IncludeSearchPaths(
                                    ctx.search_paths
                                        .iter()
                                        .map(|x| x.to_string_lossy().to_string())
                                        .collect::<Vec<_>>()
                                        .join("\n"),
                                ))
                                .with_note(NoteKind::IncludeKinds)
                        })?
                        .to_string_lossy()
                        .to_string();
                }

                let include_full_path = base_path.join(&include_file);
                let canonical_path = include_full_path
                    .canonicalize()
                    .expect("failed to canonicalize path");

                // Check if already included with pragma once
                if ctx.pragma_once_files.contains(&canonical_path) {
                    chunk_start_line = line_num + 1;
                    chunk_start_byte = current_byte + line.len() + 1; // +1 for newline
                    continue;
                }

                // Read and preprocess the included file
                let included_content = fs::read_to_string(&canonical_path).map_err(|err| {
                    let sp = Span::from_positions_and_file(
                        current_byte,
                        current_byte + line.len(),
                        static_file_name,
                    );

                    DiagnosticKind::PreprocessorCannotReadIncludeFile
                        .error_in(sp)
                        .with_label(GenericLabel::error(
                            LabelKind::PreprocessorCannotReadIncludeFile(include_file.clone())
                                .in_span(sp),
                        ))
                        .with_note(NoteKind::ReadFailed(err.to_string()))
                })?;

                // Recursively preprocess
                let include_base = canonical_path
                    .parent()
                    .expect("included file has no parent?");

                preprocess_internal(include_base, &include_file, &included_content, ctx)?;

                chunk_start_line = line_num + 1;
                chunk_start_byte = current_byte + line.len() + 1; // +1 for newline
            } else {
                let sp = Span::from_positions_and_file(
                    current_byte,
                    current_byte + line.len(),
                    static_file_name,
                );
                let mut diag = DiagnosticKind::PreprocessorUnknownDirective
                    .error_in(sp)
                    .with_label(GenericLabel::error(
                        LabelKind::PreprocessorUnknownDirective.in_span(sp),
                    ));

                if directive.starts_with("define") {
                    // i forgor which syntax it was but this is a useful note
                    diag = diag.with_note(NoteKind::MacrosNotSupported);
                }

                return Err(diag);
            }
        } else {
            current_chunk_lines.push(line);
        }

        // Update current byte position (line length + 1 for newline)
        current_byte += line.len() + 1;
    }

    // Add the file to pragma_once set if it has the directive
    if has_pragma_once && let Ok(canonical) = base_path.join(file_name).canonicalize() {
        ctx.pragma_once_files.insert(canonical);
    }

    // Flush remaining chunk
    if !current_chunk_lines.is_empty() {
        ctx.chunks.push(SourceChunk::new(
            static_file_name.to_string(),
            chunk_start_line,
            chunk_start_byte,
            current_chunk_lines.join("\n"),
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preprocess_simple_file_without_directives() {
        let content = "fn main() {\n    printf(\"Hello\");\n}";
        let chunks =
            preprocess(Path::new("."), vec![], "test.zr", content).expect("preprocessing failed");

        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].file_name, "./test.zr");
        assert_eq!(chunks[0].start_line, 1);
        assert_eq!(chunks[0].byte_offset, 0);
        assert_eq!(chunks[0].content, content);
    }

    #[test]
    fn preprocess_with_pragma_once() {
        let content = "#pragma once\nfn test() {}";
        let chunks =
            preprocess(Path::new("."), vec![], "test.zr", content).expect("preprocessing failed");

        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].start_line, 2);
        assert_eq!(chunks[0].byte_offset, 13); // "#pragma once\n" is 13 bytes
        assert_eq!(chunks[0].content, "fn test() {}");
    }

    #[test]
    fn preprocess_pragma_once_with_multiple_lines() {
        let content = "#pragma once\n\nfn first() {}\nfn second() {}";
        let chunks =
            preprocess(Path::new("."), vec![], "test.zr", content).expect("preprocessing failed");

        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].start_line, 2);
        assert_eq!(chunks[0].byte_offset, 13); // "#pragma once\n" is 13 bytes
        assert_eq!(chunks[0].content, "\nfn first() {}\nfn second() {}");
    }

    #[test]
    fn preprocess_tracks_byte_offsets_correctly() {
        // Test that byte offsets are correctly calculated
        let content = "line1\n#pragma once\nline3";
        let chunks =
            preprocess(Path::new("."), vec![], "test.zr", content).expect("preprocessing failed");

        // First chunk: "line1" (before pragma)
        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].content, "line1");
        assert_eq!(chunks[0].byte_offset, 0);
        assert_eq!(chunks[0].start_line, 1);

        // Second chunk: "line3" (after pragma)
        assert_eq!(chunks[1].content, "line3");
        assert_eq!(chunks[1].byte_offset, 19); // "line1\n#pragma once\n" is 19 bytes
        assert_eq!(chunks[1].start_line, 3);
    }
}
