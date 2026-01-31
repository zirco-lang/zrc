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

use std::{
    env,
    error::Error,
    ffi::{CString, c_char},
    fmt, iter,
    path::{Path, PathBuf},
    process,
};

use clap::Parser;
use inkwell::{
    context::Context,
    execution_engine::ExecutionEngine,
    support::{load_library_permanently, load_visible_symbols},
    targets::{CodeModel, InitializationConfig, RelocMode, Target},
};
use zrc_codegen::{cg_program, get_native_triple};
use zrc_parser::parser;
use zrc_typeck::typeck;
use zrc_utils::{io, line_finder::LineLookup};

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

/// Split an environment variable containing paths into a vector of [`PathBuf`]s
fn split_paths(var: &str) -> Vec<PathBuf> {
    env::var_os(var)
        .map(|val| env::split_paths(&val).collect())
        .unwrap_or_default()
}

/// Get the possible library filenames for a given library name on this platform
fn library_filenames(name: &str) -> Vec<String> {
    #[cfg(target_os = "linux")]
    {
        vec![format!("lib{name}.so")]
    }

    #[cfg(target_os = "macos")]
    {
        vec![format!("lib{name}.so"), format!("lib{name}.dylib")]
    }

    #[cfg(target_os = "windows")]
    {
        vec![format!("{name}.dll")]
    }
}

/// Resolve a library name to a full path by searching in the given search paths
fn resolve_library(name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
    let candidates = library_filenames(name);

    for dir in search_paths {
        for file in &candidates {
            let path = dir.join(file);
            if path.exists() {
                return Some(path);
            }
        }
    }

    None
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    if cli.version {
        println!("{}", version_string());
        return Ok(());
    }

    let Some(ref path) = cli.path else {
        return Err(Box::new(CliError("No input file specified.".into())));
    };

    // Initialize LLVM
    let ctx = Context::create();
    Target::initialize_native(&InitializationConfig::default())?;
    let triple = get_native_triple();
    let target = Target::from_triple(&triple)?;

    let target_machine = target
        .create_target_machine(
            &triple,
            "",
            "",
            cli.opt_level.into(),
            RelocMode::PIC,
            CodeModel::JITDefault,
        )
        .expect("target machine should be created successfully");

    let include_paths = cli::get_include_paths(&cli);
    let version_string = version_string();
    let cli_args = env::args().collect::<Vec<_>>().join(" ");

    let jit_module = ctx.create_module("zrxroot");

    let mut all_files: Vec<PathBuf> = Vec::with_capacity(1 + cli.extra_files.len());
    all_files.push(path.clone());
    all_files.extend(cli.extra_files.iter().cloned());

    for path in all_files {
        let (directory_name, file_name, mut input) = io::open_input(&path)?;

        let mut source_content = String::new();
        input.read_to_string(&mut source_content)?;

        let chunks = zrc_preprocessor::preprocess(
            Path::new(&directory_name),
            &include_paths,
            &file_name,
            &source_content,
        )?;

        let mut ast = Vec::new();
        for chunk in &chunks {
            let chunk_decls = parser::parse_source_chunk(chunk)?;
            ast.extend(chunk_decls);
        }

        let mut global_scope = typeck::GlobalScope::new();
        let typed_ast = typeck::type_program(&mut global_scope, ast)?;

        let file_module = cg_program(
            &version_string,
            &cli_args,
            &ctx,
            &target_machine,
            cli.opt_level.into(),
            zrc_codegen::DebugLevel::None,
            &directory_name,
            &file_name,
            &LineLookup::new(&source_content),
            typed_ast,
        );

        jit_module.link_in_module(file_module)?;
    }

    // IMPORTANT: Must call this to ensure the MCJIT components are linked in,
    // especially in release builds where link-time optimization may remove them.
    ExecutionEngine::link_in_mc_jit();

    let ee = jit_module.create_jit_execution_engine(cli.opt_level.into())?;

    // Load any libraries specified on the command line into this process
    let mut library_paths = split_paths("LD_LIBRARY_PATH");
    library_paths.extend(split_paths("DYLD_LIBRARY_PATH"));
    library_paths.extend(cli.lib_paths);

    // use inkwell::support::load_library_permanently to load each library
    for lib in &cli.libraries {
        if let Some(lib_path) = resolve_library(lib, &library_paths) {
            load_library_permanently(&lib_path)?;
        } else {
            return Err(Box::new(CliError(format!(
                "Could not find library '{lib}' in specified library paths."
            ))));
        }
    }

    // Load all other symbols visible to the current process into the JIT
    load_visible_symbols();

    // Main expects (usize, **u8) -> i32 so we must prep the extra args for it
    // Obtain the **u8 from the cli.program_args
    let c_strings: Vec<CString> =
        iter::once(CString::new("zrx-script").expect("program name contained null byte"))
            .chain(cli.program_args.iter().map(|arg| {
                CString::new(arg.as_str()).expect("program argument contained null byte")
            }))
            .collect();

    let c_ptrs: Vec<*const c_char> = c_strings.iter().map(|cstr| cstr.as_ptr()).collect();

    // SAFETY: The Zirco type checker ensures any function named "main" has the
    // correct signature
    let main = unsafe {
        ee.get_function::<unsafe extern "C" fn(usize, *const *const c_char) -> i32>("main")?
    };

    // SAFETY: We are calling a JIT-compiled function with the correct signature as
    // asserted by typeck
    let exit_code = unsafe { main.call(c_ptrs.len(), c_ptrs.as_ptr()) };

    process::exit(exit_code);
}
