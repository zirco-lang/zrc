//! Zirco compiler driver
//!
//! This module contains the main driver function for the Zirco compiler,
//! which orchestrates the parsing, type checking, and code generation phases.

use std::{
	path::{Path, PathBuf},
	time::Instant,
};

use tracing::{debug, debug_span, info};
use zrc_codegen::{DebugLevel, OptimizationLevel};
use zrc_parser::parser;
use zrc_typeck::typeck;

/// The list of possible outputs `zrc` can emit in
///
/// Usually you will want to use `llvm`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputFormat {
	/// LLVM IR
	Llvm,
	/// The Zirco AST, in Rust-like format
	AstDebug,
	/// The Zirco AST, in Rust-like format with indentation
	AstDebugPretty,
	/// The Zirco AST, stringified to Zirco code again
	///
	/// This usually looks like your code with a bunch of parenthesis added.
	Ast,
	/// The Zirco TAST, in Rust-like format
	TastDebug,
	/// The Zirco TAST, in Rust-like format with indentation
	TastDebugPretty,
	/// The Zirco TAST, stringified to Zirco code again
	///
	/// This usually looks like your code with a bunch of parenthesis added.
	Tast,
	/// Assembly
	Asm,
	/// Object file
	Object,
}

/// Drive the compilation process.
///
/// This function takes the source code as input and processes it through
/// the various stages of compilation: parsing, type checking, and code
/// generation. Depending on the specified output format, it can return the AST,
/// TAST, LLVM IR, assembly, or object code.
///
/// # Arguments
///
/// * `frontend_version_string` - A string representing the version of the
///   frontend.
/// * `include_paths` - The list of directories to search for includes.
/// * `emit` - The desired output format.
/// * `parent_directory` - The parent directory of the source file.
/// * `file_name` - The name of the source file.
/// * `cli_args` - The command line arguments passed to the compiler.
/// * `content` - The source code content to be compiled.
/// * `optimization_level` - The optimization level for code generation.
/// * `debug_mode` - The debug level for code generation.
/// * `triple` - The target triple for code generation.
/// * `cpu` - The target CPU for code generation.
/// * `forbid_unlisted_includes` - Whether to restrict includes to search paths
///   only.
///
/// # Errors
///
/// Err variant contains a [`zrc_diagnostics::Diagnostic`] if any phase of the
/// compilation fails.
#[expect(
	clippy::too_many_arguments,
	clippy::wildcard_enum_match_arm,
	clippy::result_large_err,
	clippy::too_many_lines
)]
pub fn compile(
	frontend_version_string: &str,
	include_paths: Vec<PathBuf>,
	emit: &OutputFormat,
	parent_directory: &str,
	file_name: &str,
	cli_args: &str,
	content: &str,
	optimization_level: OptimizationLevel,
	debug_mode: DebugLevel,
	triple: &zrc_codegen::TargetTriple,
	cpu: &str,
	forbid_unlisted_includes: bool,
) -> Result<Box<[u8]>, zrc_diagnostics::Diagnostic> {
	// === PREPROCESSOR ===
	info!(
		include_paths = ?include_paths,
		parent_directory = parent_directory,
		file_name = file_name,
		"running preprocessor"
	);
	let preprocessor_start = Instant::now();
	let chunks = zrc_preprocessor::preprocess(
		Path::new(parent_directory),
		include_paths,
		file_name,
		content,
		forbid_unlisted_includes,
	)?;
	debug!(
		elapsed = ?preprocessor_start.elapsed(),
		chunk_count = chunks.len(),
		"preprocessor finished"
	);

	// === PARSER ===
	let parse_start = Instant::now();
	info!("parsing source code");
	let mut ast = Vec::new();
	for chunk in &chunks {
		let _span = debug_span!(
			"parse_chunk",
			start_line = chunk.start_line,
			file_name = chunk.file_name
		)
		.entered();
		debug!("parsing chunk");
		let chunk_decls = parser::parse_source_chunk(chunk)?;
		debug!("parsed {} declarations from chunk", chunk_decls.len());
		ast.extend(chunk_decls);
	}
	debug!(elapsed = ?parse_start.elapsed(), "parsed {} declarations in total", ast.len());

	// display the AST if the user wants it
	if matches!(
		emit,
		OutputFormat::Ast | OutputFormat::AstDebug | OutputFormat::AstDebugPretty,
	) {
		return Ok(match *emit {
			OutputFormat::Ast => ast
				.into_iter()
				.map(|x| x.to_string())
				.collect::<Vec<_>>()
				.join("\n"),
			OutputFormat::AstDebug => format!("{ast:?}"),
			OutputFormat::AstDebugPretty => format!("{ast:#?}"),

			// unreachable because we test above
			_ => unreachable!(),
		}
		.as_bytes()
		.into());
	}

	// otherwise, move on:
	// === TYPE CHECKER ===
	let tck_start = Instant::now();
	info!("type checking AST");
	let mut global_scope = typeck::GlobalScope::new();
	let typed_ast = typeck::type_program(&mut global_scope, ast)?;
	debug!(elapsed = ?tck_start.elapsed(), "type checking finished successfully");

	// display the TAST if the user wants it
	if matches!(
		emit,
		OutputFormat::TastDebug | OutputFormat::TastDebugPretty | OutputFormat::Tast,
	) {
		return Ok(match *emit {
			OutputFormat::TastDebug => format!("{typed_ast:?}"),
			OutputFormat::TastDebugPretty => format!("{typed_ast:#?}"),
			OutputFormat::Tast => typed_ast
				.into_iter()
				.map(|x| x.to_string())
				.collect::<Vec<_>>()
				.join("\n"),

			// unreachable because we test above
			_ => unreachable!(),
		}
		.as_bytes()
		.into());
	}

	// otherwise, move on:
	// === CODE GENERATOR ===

	let cg_start = Instant::now();
	info!(
		optimization_level = ?optimization_level,
		debug_mode = ?debug_mode,
		triple = ?triple,
		cpu = cpu,
		"generating code"
	);
	let output: Box<[u8]> = match *emit {
		OutputFormat::Asm => zrc_codegen::cg_program_to_buffer(
			frontend_version_string,
			parent_directory,
			file_name,
			cli_args,
			content,
			typed_ast,
			zrc_codegen::FileType::Assembly,
			optimization_level,
			debug_mode,
			triple,
			cpu,
		)
		.as_slice()
		.into(),
		OutputFormat::Object => zrc_codegen::cg_program_to_buffer(
			frontend_version_string,
			parent_directory,
			file_name,
			cli_args,
			content,
			typed_ast,
			zrc_codegen::FileType::Object,
			optimization_level,
			debug_mode,
			triple,
			cpu,
		)
		.as_slice()
		.into(),

		OutputFormat::Llvm => zrc_codegen::cg_program_to_string(
			frontend_version_string,
			parent_directory,
			file_name,
			cli_args,
			content,
			typed_ast,
			optimization_level,
			debug_mode,
			triple,
			cpu,
		)
		.as_bytes()
		.into(),

		// unreachable because we return in the above cases
		_ => {
			unreachable!();
		}
	};
	debug!(
		elapsed = ?cg_start.elapsed(),
		output_size = output.len(),
		"code generation finished"
	);

	Ok(output)
}
