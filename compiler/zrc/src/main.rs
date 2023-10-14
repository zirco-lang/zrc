#![doc=include_str!("../README.md")]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::path::PathBuf;

use anyhow::Context as _;
use clap::Parser;

#[doc(hidden)]
#[allow(clippy::all)]
#[allow(clippy::nursery)]
#[allow(clippy::pedantic)]
#[allow(missing_docs)]
#[allow(clippy::missing_docs_in_private_items)]
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/shadow.rs"));
}

/// Returns the string which represents the current Zirco version
fn version() -> String {
    format!(
        "{zrc} version {version} ({commit}, {taint_string}) built for {target} on {time} ({mode} mode)\n{rust_version} ({rust_channel} on {build_os})\n{cargo_version}{taint_extra}",
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
            format!("\ntainted files: {}", build::GIT_STATUS_FILE.lines().map(|x| format!("\n{}", {
                x.strip_suffix(" (dirty)").or_else(|| x.strip_suffix(" (staged)")).unwrap_or(x)
            })).collect::<String>())   
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
        eprintln!("note: bug reporting link: https://github.com/zirco-lang/zrc/issues/new?template=ice.yml");
        eprintln!();
        eprintln!(
            "{}",
            version()
                .lines()
                .map(|line| format!("info: {line}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
        eprintln!();
        eprintln!(
            "info: command line arguments: {}",
            std::env::args().collect::<Vec<_>>().join(" ")
        );
        eprintln!();
        default_panic_hook(panic_info);
        eprintln!();
        eprintln!("error: end internal compiler error. compilation failed.");
    }));

    let cli = Cli::parse();

    if cli.version {
        println!("{}", version());
        return Ok(());
    }

    let content = std::fs::read_to_string(cli.path.context("no input file provided")?)
        .context("failed to read input file")?;

    let result = compile(&content);
    match result {
        Err(diagnostic) => eprintln!("{}", diagnostic.print(&content)),
        Ok(x) => println!("{x}"),
    }

    Ok(())
}

/// Drive the compilation process.
fn compile(content: &str) -> Result<String, zrc_diagnostics::Diagnostic> {
    Ok(zrc_codegen::cg_program(zrc_typeck::typeck::type_program(
        zrc_parser::parser::parse_program(content)?,
    )?)
    .expect("code generation should not fail"))
}
