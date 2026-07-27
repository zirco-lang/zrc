//! Execute a list of lints on a program.

use std::{
	path::{Path, PathBuf},
	time::Instant,
};

use tracing::{debug, info};
use zrc_diagnostics::Diagnostic;
use zrc_parser::parser;
use zrc_typeck::typeck;

use crate::{diagnostic::LintDiagnostic, lints, pass::PassList};

/// Lint a program with a list of [`crate::lint::Lint`]s.
#[expect(clippy::result_large_err)]
pub fn run(
	include_paths: Vec<PathBuf>,
	parent_directory: &Path,
	file_name: &str,
	content: &str,
	forbid_unlisted_includes: bool,
	passes: &PassList,
) -> Result<Vec<LintDiagnostic>, Diagnostic> {
	// This function very closely mirrors the beginning of the `compile` function
	// in `zrc/compiler/zrc/src/compile.rs`.

	let mut diagnostics = Vec::new();

	// === PREPROCESSOR ===
	info!("running preprocessor");
	let chunks = zrc_preprocessor::preprocess(
		parent_directory,
		include_paths,
		file_name,
		content,
		forbid_unlisted_includes,
	)?;

	// === PARSER ===
	info!("parsing source code");
	let mut ast = Vec::new();
	for chunk in &chunks {
		let chunk_decls = parser::parse_source_chunk(chunk)?;
		ast.extend(chunk_decls);
	}

	// Execute syntactic lints
	info!("running lints on AST");
	let ast_start = Instant::now();
	diagnostics.extend(passes.lint_ast(&ast));
	debug!(elapsed = ?ast_start.elapsed(), "lints on AST finished");

	// === TYPE CHECKER ===
	info!("type checking source code");
	let mut global_scope = typeck::GlobalScope::new();
	let typed_ast = typeck::type_program(&mut global_scope, ast)?;

	// Execute semantic lints
	info!("running lints on TAST");
	let tast_start = Instant::now();
	diagnostics.extend(passes.lint_tast(&typed_ast));
	debug!(elapsed = ?tast_start.elapsed(), "lints on TAST finished");

	Ok(diagnostics)
}

/// Lint a program using the default passes listed in
/// [`crate::lints::get_default_lints`].
#[expect(clippy::result_large_err)]
pub fn run_with_default_passes(
	include_paths: Vec<PathBuf>,
	parent_directory: &Path,
	file_name: &str,
	content: &str,
	forbid_unlisted_includes: bool,
) -> Result<Vec<LintDiagnostic>, Diagnostic> {
	let passes = lints::get_default_lints();
	run(
		include_paths,
		parent_directory,
		file_name,
		content,
		forbid_unlisted_includes,
		&passes,
	)
}
