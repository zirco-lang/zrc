//! Defines the zrc command line interface

use std::path::PathBuf;

use clap::Parser;
use derive_more::Display;
use zrc_codegen::OptimizationLevel;

/// The official Zirco compiler
#[derive(Parser)]
#[command(version=None)]
pub struct Cli {
    /// See what version of zrc you are using
    #[arg(short, long)]
    pub version: bool,

    /// The path of the file to compile
    pub path: Option<PathBuf>,

    /// The path of the file to write the output to
    /// If not provided, the output will be written to stdout
    #[arg(short, long)]
    #[clap(default_value = "-")]
    pub out_file: PathBuf,

    /// What output format to emit
    #[arg(long)]
    #[clap(default_value_t = OutputFormat::Llvm)]
    pub emit: OutputFormat,

    /// Allow emitting raw object code to stdout. This may mess up your
    /// terminal!
    #[arg(long)]
    pub force: bool,

    /// Set the target triple to generate output for. Defaults to native.
    #[arg(short, long)]
    pub target: Option<String>,

    /// Set the target CPU to generate output for.
    #[arg(long)]
    #[clap(default_value = "generic")]
    pub cpu: String,

    /// Set the optimization level
    #[arg(short = 'O', long = "opt-level")]
    #[clap(default_value = "default")]
    pub opt_level: FrontendOptLevel,

    /// Enable debugging information
    #[arg(short = 'g')]
    pub debug: bool,
}

/// Configuration for the Zirco optimizer
#[derive(Debug, Clone, clap::ValueEnum, PartialEq, Eq)]
pub enum FrontendOptLevel {
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
#[derive(Debug, Clone, clap::ValueEnum, PartialEq, Eq, Display)]
pub enum OutputFormat {
    /// LLVM IR
    #[display("llvm")]
    Llvm,
    /// The Zirco AST, in Rust-like format
    #[display("ast-debug")]
    AstDebug,
    /// The Zirco AST, in Rust-like format with indentation
    #[display("ast-debug-pretty")]
    AstDebugPretty,
    /// The Zirco AST, stringified to Zirco code again
    ///
    /// This usually looks like your code with a bunch of parenthesis added.
    #[display("ast")]
    Ast,
    /// The Zirco TAST, in Rust-like format
    #[display("tast-debug")]
    TastDebug,
    /// The Zirco TAST, in Rust-like format with indentation
    #[display("tast-debug-pretty")]
    TastDebugPretty,
    /// Assembly
    #[display("asm")]
    Asm,
    /// Object file
    #[display("object")]
    Object,
}
