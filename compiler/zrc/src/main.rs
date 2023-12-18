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
    variant_size_differences,
    unused_qualifications,
    clippy::unwrap_used
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::{
    fmt::{Display, Write},
    path::PathBuf,
};

use anyhow::bail;
use clap::Parser;
use zrc_codegen::{DebugLevel, OptimizationLevel};

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
                        .expect("writing to a string should succeed");
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

    /// Allow emitting raw object code to stdout. This may mess up your
    /// terminal!
    #[arg(long)]
    force: bool,

    /// Set the target triple to generate output for. Defaults to native.
    #[arg(short, long)]
    target: Option<String>,

    /// Set the target CPU to generate output for.
    #[arg(long)]
    #[clap(default_value = "generic")]
    cpu: String,

    /// Set the optimization level
    #[arg(short = 'O', long = "opt-level")]
    #[clap(default_value = "default")]
    opt_level: FrontendOptLevel,

    /// Enable debugging information
    #[arg(short = 'g')]
    debug: bool,
}

/// Configuration for the Zirco optimizer
#[derive(Clone, clap::ValueEnum, PartialEq)]
enum FrontendOptLevel {
    /// Disable as many optimizations as possible.
    #[value(name = "0", alias("none"))]
    O0,
    /// Optimize quickly without destroying debuggability.
    #[value(name = "1")]
    O1,
    /// Optimize for fast execution as much as possible without triggering
    /// significant incremental compile time or code size growth.
    #[value(name = "2", alias("default"))]
    O2,
    /// Optimize for fast execution as much as possible.
    // TODO: does this enable LTO?
    #[value(name = "3", alias("aggressive"))]
    O3,
}
impl From<FrontendOptLevel> for OptimizationLevel {
    fn from(val: FrontendOptLevel) -> Self {
        match val {
            FrontendOptLevel::O0 => Self::None,
            FrontendOptLevel::O1 => Self::Less,
            FrontendOptLevel::O2 => Self::Default,
            FrontendOptLevel::O3 => Self::Aggressive,
        }
    }
}

/// The list of possible outputs `zrc` can emit in
///
/// Usually you will want to use `llvm`.
#[derive(Clone, clap::ValueEnum, PartialEq)]
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
    /// Assembly
    Asm,
    /// Object file
    Object,
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
            Self::Asm => write!(f, "asm"),
            Self::Object => write!(f, "object"),
        }
    }
}

#[allow(clippy::too_many_lines)]
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

    let directory_name: String;
    let file_name: String;
    let mut input: Box<dyn std::io::Read + 'static> = if path.clone().into_os_string() == "-" {
        directory_name = "/dev".to_string();
        file_name = "stdin".to_string();
        Box::new(std::io::stdin())
    } else {
        match std::fs::File::open(&path) {
            Ok(file) => {
                let mut canonical = std::fs::canonicalize(path)?;
                file_name = canonical
                    .file_name()
                    .expect("file name should exist")
                    .to_str()
                    .expect("should be a valid str")
                    .to_string();
                canonical.pop();

                directory_name = canonical
                    .to_str()
                    .expect("directory should be a valid str")
                    .to_string();

                Box::new(file)
            }
            Err(err) => {
                bail!(err);
            }
        }
    };

    let mut content = String::new();
    input.read_to_string(&mut content)?;

    if cli.emit == OutputFormat::Object
        && !cli.force
        && cli.out_file.as_os_str().to_str().unwrap_or("-") == "-"
    {
        bail!("emitting raw object code to stdout is not allowed. use --force to override this");
    }

    let result = compile(
        &version(),
        &cli.emit,
        &directory_name,
        &file_name,
        &std::env::args().collect::<Vec<_>>().join(" "),
        &content,
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

            output
                .write_all(&x)
                .expect("writing to stdout or file should succeed");
        }
    }

    Ok(())
}

/// Drive the compilation process.
#[allow(clippy::too_many_arguments, clippy::wildcard_enum_match_arm)]
fn compile(
    frontend_version_string: &str,
    emit: &OutputFormat,
    parent_directory: &str,
    file_name: &str,
    cli_args: &str,
    content: &str,
    optimization_level: OptimizationLevel,
    debug_mode: DebugLevel,
    triple: &zrc_codegen::TargetTriple,
    cpu: &str,
) -> Result<Box<[u8]>, zrc_diagnostics::Diagnostic> {
    // === PARSER ===
    let ast = zrc_parser::parser::parse_program(content)?;

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
    let typed_ast = zrc_typeck::typeck::type_program(ast)?;

    // display the TAST if the user wants it
    if matches!(
        emit,
        OutputFormat::Tast | OutputFormat::TastDebug | OutputFormat::TastDebugPretty,
    ) {
        return Ok(match *emit {
            OutputFormat::Tast => typed_ast
                .into_iter()
                .map(|x| x.value().to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            OutputFormat::TastDebug => format!("{typed_ast:?}"),
            OutputFormat::TastDebugPretty => format!("{typed_ast:#?}"),

            // unreachable because we test above
            _ => unreachable!(),
        }
        .as_bytes()
        .into());
    }

    // otherwise, move on:
    // === CODE GENERATOR ===

    match *emit {
        OutputFormat::Asm => Ok(zrc_codegen::cg_program_to_buffer(
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
        .into()),
        OutputFormat::Object => Ok(zrc_codegen::cg_program_to_buffer(
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
        .into()),

        OutputFormat::Llvm => Ok(zrc_codegen::cg_program_to_string(
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
        .into()),

        // unreachable because we return in the above cases
        _ => unreachable!(),
    }
}
