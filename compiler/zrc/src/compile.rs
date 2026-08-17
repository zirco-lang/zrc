//! Zirco compiler driver
//!
//! This module contains the main driver function for the Zirco compiler,
//! which orchestrates the parsing, type checking, and code generation phases.

use std::{path::PathBuf, time::Instant};

use tracing::{debug, debug_span, info};
use zrc_codegen::{CgProgramInputs, DebugLevel, FileType, OptimizationLevel};
use zrc_parser::parser;
use zrc_preprocessor::PreprocessInputs;
use zrc_typeck::typeck;

/// The list of possible output file types `zrc` can emit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// The inputs to the Zirco [`compile`] compilation driver.
#[derive(Debug)]
pub struct CompileInputs<'a> {
	/// A version string for the frontend, used in debug info.
	pub frontend_version_string: &'a str,
	/// The CLI arguments passed to the compiler, used in debug info.
	pub cli_args: &'a str,
	/// All search paths for global `#include` directives
	pub include_paths: &'a Vec<PathBuf>,
	/// The desired output format.
	pub emit: OutputFormat,
	/// The path of the source file.
	pub path: &'a PathBuf,
	/// The optimization level for code generation.
	pub optimization_level: OptimizationLevel,
	/// The debug level for code generation.
	pub debug_mode: DebugLevel,
	/// The target triple for code generation.
	pub triple: &'a zrc_codegen::TargetTriple,
	/// The target CPU for code generation.
	pub cpu: &'a str,
	/// Whether to restrict includes to search paths only.
	pub forbid_unlisted_includes: bool,
	/// The content of the source file.
	pub content: &'a str,
}

impl<'a> From<&CompileInputs<'a>> for PreprocessInputs<'a> {
	fn from(val: &CompileInputs<'a>) -> Self {
		PreprocessInputs {
			path: val.path,
			content: val.content,
			include_paths: val.include_paths,
			forbid_unlisted_includes: val.forbid_unlisted_includes,
		}
	}
}

impl<'a> From<CompileInputs<'a>> for CgProgramInputs<'a> {
	fn from(val: CompileInputs<'a>) -> Self {
		CgProgramInputs {
			frontend_version_string: val.frontend_version_string,
			cli_args: val.cli_args,
			path: val.path,
			source: val.content,
			optimization_level: val.optimization_level,
			debug_level: val.debug_mode,
			triple: val.triple,
			cpu: val.cpu,
			#[expect(clippy::wildcard_enum_match_arm, clippy::match_same_arms)]
			file_type: match val.emit {
				OutputFormat::Asm => FileType::Assembly,
				OutputFormat::Object => FileType::Object,
				// this value will luckily never be needed but is still required
				_ => FileType::Assembly,
			},
		}
	}
}

/// Drive the compilation process.
///
/// This function takes the source code as input and processes it through
/// the various stages of compilation: parsing, type checking, and code
/// generation. Depending on the specified output format, it can return the AST,
/// TAST, LLVM IR, assembly, or object code.
///
/// # Errors
///
/// Err variant contains a [`zrc_diagnostics::Diagnostic`] if any phase of the
/// compilation fails.
#[expect(clippy::wildcard_enum_match_arm, clippy::result_large_err)]
pub fn compile(
	inputs @ CompileInputs { emit, .. }: CompileInputs<'_>,
) -> Result<Box<[u8]>, zrc_diagnostics::Diagnostic> {
	// === PREPROCESSOR ===
	info!("running preprocessor");
	let preprocessor_start = Instant::now();
	let chunks = zrc_preprocessor::preprocess((&inputs).into())?;
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
		return Ok(match emit {
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
		return Ok(match emit {
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
	info!("generating code");
	let output: Box<[u8]> = match emit {
		OutputFormat::Asm | OutputFormat::Object => {
			zrc_codegen::cg_program_to_buffer(inputs.into(), typed_ast)
				.as_slice()
				.into()
		}

		OutputFormat::Llvm => zrc_codegen::cg_program_to_string(inputs.into(), typed_ast)
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
