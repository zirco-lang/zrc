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

mod build_info;
mod cli;

use std::{collections::HashSet, error::Error, fmt, path::Path, process};

use clap::Parser;
use cli::Cli;
use zircop::{ignore, runner};
use zrc_utils::{io, line_finder::LineLookup};

/// An error produced by the zircop CLI
#[derive(Debug)]
struct CliError(String);
impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for CliError {}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    if cli.version {
        println!("{}", build_info::VERSION_STRING);
        return Ok(());
    }

    let Some(ref path) = cli.path else {
        return Err(Box::new(CliError("No input file specified.".into())));
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
            // Parse in-code ignore directives
            let in_code_ignores = ignore::parse_ignore_directives(&source_content);
            let line_lookup = LineLookup::new(&source_content);

            // Convert CLI allowed lints to a HashSet for O(1) lookups
            let cli_allowed: HashSet<&str> = cli.allowed_lints.iter().map(String::as_str).collect();

            // Filter out allowed lints (both CLI and in-code)
            let filtered_diagnostics: Vec<_> = diagnostics
                .into_iter()
                .filter(|diag| {
                    let lint_name = diag.kind().value().name();

                    // Check CLI allows
                    if cli_allowed.contains(lint_name) || cli_allowed.contains("all") {
                        return false;
                    }

                    // Check in-code ignores
                    let span = diag.kind().span();
                    let line_and_col = line_lookup.lookup_from_index(span.start());
                    // SAFETY: u32 to usize cast is always safe on 32-bit and 64-bit systems
                    #[expect(clippy::as_conversions)]
                    let line = line_and_col.line as usize;

                    if let Some(ignored_lints) = in_code_ignores.get(&line)
                        && (ignored_lints
                            .iter()
                            .any(|lint| lint == lint_name || lint == "all"))
                    {
                        return false;
                    }

                    true
                })
                .collect();

            for diag in &filtered_diagnostics {
                eprintln!("{}", diag.print(Some(&source_content)));
            }

            println!(
                "Linting complete: {} issue(s) found.",
                filtered_diagnostics.len()
            );

            if !filtered_diagnostics.is_empty() {
                process::exit(1);
            }
        }
    }

    Ok(())
}
