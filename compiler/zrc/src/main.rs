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
    clippy::module_name_repetitions,
    clippy::doc_comment_double_space_linebreaks
)]

use mimalloc::MiMalloc;
/// Use the mimalloc allocator as the global allocator, as LLVM is heavy on heap
/// allocations as a result of its OOP design. Some comparisons have shown
/// mimalloc to be 10% faster than the default system allocator in real-world
/// applications, and it also has better memory usage characteristics.
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

mod build_info;
mod cli;
mod compile;
mod ice;

use anyhow::bail;
use clap::Parser;
use cli::{Cli, OutputFormat};
use zrc_codegen::DebugLevel;
use zrc_utils::io;

fn main() -> anyhow::Result<()> {
    ice::setup_panic_hook();

    let cli = Cli::parse();

    if cli.version {
        println!("{}", build_info::version());
        return Ok(());
    }

    let Some(ref path) = cli.path else {
        bail!("no input file provided");
    };

    let (directory_name, file_name, mut input) = io::open_input(path)?;

    let mut source_content = String::new();
    input.read_to_string(&mut source_content)?;

    if cli.emit == OutputFormat::Object
        && !cli.force
        && cli.out_file.as_os_str().to_str().unwrap_or("-") == "-"
    {
        bail!("emitting raw object code to stdout is not allowed. use --force to override this");
    }

    let result = compile::compile(
        &build_info::version(),
        cli::get_include_paths(&cli),
        &cli.emit,
        &directory_name,
        &file_name,
        &std::env::args().collect::<Vec<_>>().join(" "),
        &source_content,
        cli.opt_level.into(),
        if cli.debug {
            DebugLevel::Full
        } else {
            DebugLevel::None
        },
        &cli.target
            .map_or_else(zrc_codegen::get_native_triple, |triple| {
                zrc_codegen::TargetTriple::create(&triple)
            }),
        &cli.cpu,
        cli.timings,
    );

    match result {
        Err(diagnostic) => {
            eprintln!("{}", diagnostic.print(Some(&source_content)));
            std::process::exit(1);
        }
        Ok(x) => {
            io::open_output(&cli.out_file)?.write_all(&x)?;
        }
    }

    Ok(())
}
