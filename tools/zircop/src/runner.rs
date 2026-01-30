//! Execute a list of lints on a program.

use std::path::Path;

use zrc_diagnostics::Diagnostic;
use zrc_parser::parser;
use zrc_typeck::typeck;

use crate::{diagnostic::LintDiagnostic, lints, pass::PassList};

/// Lint a program with a list of [`crate::lint::Lint`]s.
#[expect(clippy::result_large_err)]
pub fn run(
    include_paths: &[&'static Path],
    parent_directory: &Path,
    file_name: &str,
    content: &str,
    passes: &PassList,
) -> Result<Vec<LintDiagnostic>, Diagnostic> {
    // This function very closely mirrors the beginning of the `compile` function
    // in `zrc/compiler/zrc/src/compile.rs`.

    let mut diagnostics = Vec::new();

    // === PREPROCESSOR ===
    let chunks = zrc_preprocessor::preprocess(parent_directory, include_paths, file_name, content)?;

    // === PARSER ===
    let mut ast = Vec::new();
    for chunk in &chunks {
        let chunk_decls = parser::parse_source_chunk(chunk)?;
        ast.extend(chunk_decls);
    }

    // Execute syntactic lints
    diagnostics.extend(passes.lint_ast(&ast));

    // === TYPE CHECKER ===
    let mut global_scope = typeck::GlobalScope::new();
    let typed_ast = typeck::type_program(&mut global_scope, ast)?;

    // Execute semantic lints
    diagnostics.extend(passes.lint_tast(&typed_ast));

    Ok(diagnostics)
}

/// Lint a program using the default passes listed in
/// [`crate::lints::get_default_lints`].
#[expect(clippy::result_large_err)]
pub fn run_with_default_passes(
    include_paths: &[&'static Path],
    parent_directory: &Path,
    file_name: &str,
    content: &str,
) -> Result<Vec<LintDiagnostic>, Diagnostic> {
    let passes = lints::get_default_lints();
    run(include_paths, parent_directory, file_name, content, &passes)
}
