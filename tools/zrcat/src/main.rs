#![doc = "zrcat - Zirco C Auto Translator\n\nTranslates C header files to Zirco .zh equivalents."]
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

/// CLI argument parsing module
mod cli;
/// C to Zirco translator module
mod translator;

use std::{
    error::Error,
    fmt, fs,
    io::{self, Read},
    process,
};

use clap::Parser;
use cli::Cli;

/// An error produced by the zrcat CLI
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
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Read input from file or stdin
    let input = if let Some(ref path) = cli.input {
        fs::read_to_string(path)?
    } else if cli.stdin {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    } else {
        return Err(Box::new(CliError(
            "No input specified. Use --stdin or provide a file path.".into(),
        )));
    };

    // Preprocess if requested
    let preprocessed = if cli.preprocess {
        let temp_file = "/tmp/zrcat_input.h";
        fs::write(temp_file, &input)?;

        let output = process::Command::new("cpp").arg(temp_file).output()?;

        if !output.status.success() {
            return Err(Box::new(CliError(format!(
                "Preprocessing failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ))));
        }

        String::from_utf8_lossy(&output.stdout).to_string()
    } else {
        input
    };

    // Parse and translate
    let output = translator::translate(&preprocessed);

    if let Some(ref path) = cli.output {
        fs::write(path, output)?;
    } else {
        println!("{output}");
    }

    Ok(())
}
