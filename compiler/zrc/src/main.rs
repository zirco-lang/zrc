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
    clippy::string_to_string,
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
    variant_size_differences
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::{
    fmt::{Display, Write},
    path::PathBuf,
};

use anyhow::bail;
use clap::Parser;

#[doc(hidden)]
#[allow(
    clippy::all,
    clippy::nursery,
    clippy::pedantic,
    missing_docs,
    clippy::missing_docs_in_private_items,
    clippy::restriction
)]
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/shadow.rs"));
}

/// Returns the string which represents the current Zirco version
fn version() -> String {
    format!(
        concat!(
            "{zrc} version {version} ({commit}, {taint_string}) built for {target} on {time}",
            " ({mode} mode)",
            "\n{rust_version} ({rust_channel} on {build_os})\n",
            "{cargo_version}{taint_extra}"
        ),
        zrc = build::PROJECT_NAME,
        version = build::PKG_VERSION,
        commit = build::COMMIT_HASH,
        taint_string = if build::GIT_CLEAN {
            "not tainted"
        } else {
            "tainted!"
        },
        target = build::BUILD_TARGET,
        time = build::BUILD_TIME_3339,
        mode = build::BUILD_RUST_CHANNEL,
        rust_version = build::RUST_VERSION,
        rust_channel = build::RUST_CHANNEL,
        build_os = build::BUILD_OS,
        cargo_version = build::CARGO_VERSION,
        taint_extra = if build::GIT_CLEAN {
            String::new()
        } else {
            // git is tainted
            format!(
                "\ntainted files:{}",
                build::GIT_STATUS_FILE
                    .lines()
                    .fold(String::new(), |mut output, x| {
                        write!(output, "\n{}", {
                            x.strip_suffix(" (dirty)")
                                .or_else(|| x.strip_suffix(" (staged)"))
                                .unwrap_or(x)
                        })
                        .unwrap();
                        output
                    })
            )
        }
    )
}

/// The official Zirco compiler
#[derive(Parser)]
#[command(version=None)]
struct Cli {
    /// See what version of zrc you are using
    #[arg(short, long)]
    version: bool,

    /// The path of the file to compile
    path: Option<PathBuf>,

    /// The path of the file to write the output to
    /// If not provided, the output will be written to stdout
    #[arg(short, long)]
    #[clap(default_value = "-")]
    out_file: PathBuf,

    /// What output format to emit
    #[arg(long)]
    #[clap(default_value_t = OutputFormat::Llvm)]
    emit: OutputFormat,
}

/// The list of possible outputs `zrc` can emit in
///
/// Usually you will want to use `llvm`.
#[derive(Clone, clap::ValueEnum)]
enum OutputFormat {
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
    /// The Zirco TAST, stringified to Zirco-like code
    Tast,
}
impl Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Llvm => write!(f, "llvm"),
            Self::AstDebug => write!(f, "ast-debug"),
            Self::AstDebugPretty => write!(f, "ast-debug-pretty"),
            Self::Ast => write!(f, "ast"),
            Self::TastDebug => write!(f, "tast-debug"),
            Self::TastDebugPretty => write!(f, "tast-debug-pretty"),
            Self::Tast => write!(f, "tast"),
        }
    }
}

fn main() -> anyhow::Result<()> {
    let default_panic_hook = std::panic::take_hook();

    // ICE (internal compiler error) / panic message
    std::panic::set_hook(Box::new(move |panic_info| {
        eprintln!("error: internal compiler error encountered: thread panicked");
        eprintln!("note: this is not your fault! this is ALWAYS a compiler bug.");
        eprintln!(
            "note: compiler bugs threaten the Zirco ecosystem -- we would appreciate a bug report:"
        );
        eprintln!(concat!(
            "note: bug reporting link:",
            " https://github.com/zirco-lang/zrc/issues/new?template=ice.yml"
        ));
        eprintln!();
        eprintln!(
            "{}",
            version()
                .lines()
                .map(|line| format!("note: {line}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
        eprintln!();
        eprintln!(
            "note: command line arguments: {}",
            std::env::args().collect::<Vec<_>>().join(" ")
        );
        eprintln!();
        default_panic_hook(panic_info);
        eprintln!();
        eprintln!("error: end internal compiler error. compilation failed.");
    }));

    // Force RUST_BACKTRACE=1 if the user did not set RUST_BACKTRACE=full
    std::env::var("RUST_BACKTRACE").ok().map_or_else(
        || {
            // if RUST_BACKTRACE is not set, set it to 1
            std::env::set_var("RUST_BACKTRACE", "1");
        },
        |x| {
            // if RUST_BACKTRACE is not set to full, set it to 1
            if x != "full" {
                std::env::set_var("RUST_BACKTRACE", "1");
            }
        },
    );

    let cli = Cli::parse();

    if cli.version {
        println!("{}", version());
        return Ok(());
    }

    let Some(path) = cli.path else {
        bail!("no input file provided");
    };

    let mut input: Box<dyn std::io::Read + 'static> = if path.clone().into_os_string() == "-" {
        Box::new(std::io::stdin())
    } else {
        match std::fs::File::open(&path) {
            Ok(file) => Box::new(file),
            Err(err) => {
                bail!(err);
            }
        }
    };

    let mut content = String::new();
    input.read_to_string(&mut content)?;

    let result = compile(
        &cli.emit,
        match path.clone().into_os_string().to_str() {
            Some("-") => "<stdin>",
            _ => path
                .as_os_str()
                .to_str()
                .expect("Invalid UTF-8 in file name"),
        },
        &content,
    );
    match result {
        Err(diagnostic) => eprintln!("{}", diagnostic.print(&content)),
        Ok(x) => {
            let mut output: Box<dyn std::io::Write> =
                if cli.out_file.clone().into_os_string() == "-" {
                    Box::new(std::io::stdout())
                } else {
                    match std::fs::OpenOptions::new()
                        .write(true)
                        .truncate(true)
                        .create(true)
                        .open(cli.out_file)
                    {
                        Ok(file) => Box::new(file),
                        Err(err) => {
                            bail!(err);
                        }
                    }
                };

            write!(output, "{x}")?;
        }
    }

    Ok(())
}

/// Drive the compilation process.
fn compile(
    emit: &OutputFormat,
    module_name: &str,
    content: &str,
) -> Result<String, zrc_diagnostics::Diagnostic> {
    match *emit {
        OutputFormat::Llvm => Ok(zrc_codegen::cg_program(
            module_name,
            zrc_typeck::typeck::type_program(zrc_parser::parser::parse_program(content)?)?,
        )),

        OutputFormat::Ast => Ok(zrc_parser::parser::parse_program(content)?
            .into_iter()
            .map(|x| format!("{}", x.into_value()))
            .collect::<Vec<_>>()
            .join("\n")),
        OutputFormat::AstDebug => Ok(format!("{:?}", zrc_parser::parser::parse_program(content)?)),
        OutputFormat::AstDebugPretty => Ok(format!(
            "{:#?}",
            zrc_parser::parser::parse_program(content)?
        )),

        OutputFormat::Tast => Ok(zrc_typeck::typeck::type_program(
            zrc_parser::parser::parse_program(content)?,
        )?
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")),
        OutputFormat::TastDebug => Ok(format!(
            "{:?}",
            zrc_typeck::typeck::type_program(zrc_parser::parser::parse_program(content)?)?
        )),
        OutputFormat::TastDebugPretty => Ok(format!(
            "{:#?}",
            zrc_typeck::typeck::type_program(zrc_parser::parser::parse_program(content)?)?
        )),
    }
}
