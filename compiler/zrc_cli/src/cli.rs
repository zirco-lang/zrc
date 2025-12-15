//! Command line interface declarations for the Zirco compiler

use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use clap::Parser;
use zrc::{OutputFormat, codegen::OptimizationLevel};
use zrc_diagnostics::DiagnosticEmitFormat;

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
    #[clap(default_value_t = FrontendOutputFormat::Llvm)]
    pub emit: FrontendOutputFormat,

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

    /// Add a directory to the include path
    #[arg(short = 'I', long = "include", action = clap::ArgAction::Append)]
    pub include_paths: Vec<PathBuf>,

    /// Set the diagnostic output format
    #[arg(long = "diagnostic-emit")]
    #[clap(default_value = "human")]
    pub diagnostic_emit: DiagnosticFormat,
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
#[derive(Debug, Clone, clap::ValueEnum, PartialEq, Eq)]
pub enum FrontendOutputFormat {
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
    /// The Zirco TAST, stringified to Zirco code again
    ///
    /// This usually looks like your code with a bunch of parenthesis added.
    Tast,
    /// Assembly
    Asm,
    /// Object file
    Object,
}
impl Display for FrontendOutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Llvm => "llvm",
                Self::AstDebug => "ast-debug",
                Self::AstDebugPretty => "ast-debug-pretty",
                Self::Ast => "ast",
                Self::TastDebug => "tast-debug",
                Self::TastDebugPretty => "tast-debug-pretty",
                Self::Tast => "tast",
                Self::Asm => "asm",
                Self::Object => "object",
            }
        )
    }
}

impl From<FrontendOutputFormat> for OutputFormat {
    fn from(val: FrontendOutputFormat) -> Self {
        match val {
            FrontendOutputFormat::Llvm => Self::Llvm,
            FrontendOutputFormat::AstDebug => Self::AstDebug,
            FrontendOutputFormat::AstDebugPretty => Self::AstDebugPretty,
            FrontendOutputFormat::Ast => Self::Ast,
            FrontendOutputFormat::TastDebug => Self::TastDebug,
            FrontendOutputFormat::TastDebugPretty => Self::TastDebugPretty,
            FrontendOutputFormat::Tast => Self::Tast,
            FrontendOutputFormat::Asm => Self::Asm,
            FrontendOutputFormat::Object => Self::Object,
        }
    }
}

/// The format to emit diagnostics in
#[derive(Debug, Clone, Copy, clap::ValueEnum, PartialEq, Eq)]
pub enum DiagnosticFormat {
    /// Human-readable text format (default)
    Human,
    /// JSON format for machine consumption
    Json,
}
impl From<DiagnosticFormat> for DiagnosticEmitFormat {
    fn from(val: DiagnosticFormat) -> Self {
        match val {
            DiagnosticFormat::Human => Self::Human,
            DiagnosticFormat::Json => Self::Json,
        }
    }
}

/// Resolve a path to an absolute path based on the current working directory.
///
/// If the path is relative, it is resolved to an absolute path by joining it
/// with the current working directory and canonicalizing it. If the path is
/// already absolute or canonicalization fails, the path is returned as-is.
fn resolve_include_path(path: &Path) -> PathBuf {
    if path.is_relative() {
        std::env::current_dir()
            .ok()
            .and_then(|cwd| cwd.join(path).canonicalize().ok())
            .unwrap_or_else(|| path.to_path_buf())
    } else {
        path.to_path_buf()
    }
}

/// Get the include paths from the CLI environment and -I arguments
///
/// Relative paths are resolved relative to the current working directory.
pub fn get_include_paths(cli: &Cli) -> Vec<&'static Path> {
    // append paths in the following order:
    // 1. CLI
    // 2. ZIRCO_INCLUDE_PATH env var
    let mut include_paths: Vec<&'static Path> = Vec::new();

    for path in &cli.include_paths {
        let resolved_path = resolve_include_path(path);

        // SAFETY: we leak the PathBuf to get a 'static lifetime
        let static_path: &'static Path = Box::leak(resolved_path.into_boxed_path());
        include_paths.push(static_path);
    }

    if let Ok(env_paths) = std::env::var("ZIRCO_INCLUDE_PATH") {
        for path_str in std::env::split_paths(&env_paths) {
            let resolved_path = resolve_include_path(&path_str);

            // SAFETY: we leak the PathBuf to get a 'static lifetime
            let static_path: &'static Path = Box::leak(resolved_path.into_boxed_path());
            include_paths.push(static_path);
        }
    }

    include_paths
}
