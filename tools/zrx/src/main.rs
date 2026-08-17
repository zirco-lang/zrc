#![doc=include_str!("../README.md")]
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
	clippy::unwrap_used
)]
#![allow(
	clippy::multiple_crate_versions,
	clippy::cargo_common_metadata,
	unused_crate_dependencies,
	clippy::module_name_repetitions,
	clippy::doc_comment_double_space_linebreaks,
	clippy::missing_errors_doc
)]

use std::{env, error::Error, fmt, path::PathBuf, process};

use clap::Parser;
use tracing::{debug, debug_span};
use tracing_subscriber::EnvFilter;
use zrc_jit::engine::JitEngine;
use zrc_parser::parser;
use zrc_preprocessor::PreprocessInputs;
use zrc_typeck::typeck;
use zrc_utils::io;

use crate::cli::Cli;

mod cli;

/// Get the current zrx version.
fn version_string() -> String {
	zrc_buildinfo::generate_version_string(env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
}

/// An error produced by the zrx CLI
#[derive(Debug)]
struct CliError(String);
impl fmt::Display for CliError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
impl Error for CliError {}

fn main() -> Result<(), Box<dyn Error>> {
	let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"));
	tracing_subscriber::fmt().with_env_filter(filter).init();

	let cli = Cli::parse();

	debug!(parsed_args = ?cli, "finished parsing command line arguments");

	if cli.version {
		println!("{}", version_string());
		return Ok(());
	}

	let Some(ref path) = cli.path else {
		return Err(Box::new(CliError("No input file specified.".into())));
	};

	let include_paths = cli::get_include_paths(&cli);
	let version_string: &'static str = Box::leak(Box::new(version_string()));
	let cli_args: &'static str = Box::leak(Box::new(env::args().collect::<Vec<_>>().join(" ")));

	debug!("initializing Zirco JIT");
	let engine = JitEngine::init(version_string, cli_args, cli.lib_paths);
	let module = engine.create_module();

	let mut all_files: Vec<PathBuf> = Vec::with_capacity(1 + cli.extra_files.len());
	all_files.push(path.clone());
	all_files.extend(cli.extra_files.iter().cloned());

	for path in all_files {
		let _span = debug_span!("jit_file", path = ?path).entered();

		let mut input = io::open_input(&path)?;

		let mut source_content = String::new();
		input.read_to_string(&mut source_content)?;

		let zpp_inputs = PreprocessInputs {
			path: &path,
			content: &source_content,
			include_paths: &include_paths,
			forbid_unlisted_includes: false,
		};
		let chunks = zrc_preprocessor::preprocess(zpp_inputs)?;

		let mut ast = Vec::new();
		for chunk in &chunks {
			let chunk_decls = parser::parse_source_chunk(chunk)?;
			ast.extend(chunk_decls);
		}

		let mut global_scope = typeck::GlobalScope::new();
		let typed_ast = typeck::type_program(&mut global_scope, ast)?;

		module.cg_program_and_link(&path, &source_content, typed_ast);
	}

	for lib in &cli.libraries {
		engine.load_library(lib)?;
	}
	engine.load_visible_symbols();

	debug!("running main function");
	let exit_code = module.run_main(cli.program_args)?;

	process::exit(exit_code);
}
