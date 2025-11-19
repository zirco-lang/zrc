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

mod cli;

use std::{path::Path, process};

use anyhow::bail;
use clap::Parser;
use cli::Cli;
use zircop::runner;
use zrc_utils::io;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let Some(ref path) = cli.path else {
        bail!("Error: No input file specified.");
    };

    let (directory_name, file_name, mut input) = io::open_input(path)?;

    let mut source_content = String::new();
    input.read_to_string(&mut source_content)?;

    let diagnostics = runner::run_with_default_passes(
        cli::get_include_paths(&cli),
        Path::new(&directory_name),
        &file_name,
        &source_content,
    );

    match diagnostics {
        Err(diagnostic) => {
            eprintln!("{}", diagnostic.print(Some(&source_content)));
            eprintln!("The above error originated from zrc - this is not a Zircop lint.");
            process::exit(1);
        }
        Ok(diagnostics) => {
            for diag in &diagnostics {
                eprintln!("{}", diag.print(Some(&source_content)));
            }

            println!("Linting complete: {} issue(s) found.", diagnostics.len());

            if !diagnostics.is_empty() {
                process::exit(1);
            }
        }
    }

    Ok(())
}
