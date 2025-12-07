//! Command line interface declarations for the Zirco compiler

use std::path::{Path, PathBuf};

use clap::Parser;
use derive_more::Display;
use zrc_codegen::OptimizationLevel;

/// The official Zirco compiler
#[derive(Parser)]
#[command(version=None)]
#[allow(clippy::struct_excessive_bools)]
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

    /// Add a directory to the include path
    #[arg(short = 'I', long = "include", action = clap::ArgAction::Append)]
    pub include_paths: Vec<PathBuf>,

    /// Display timing information for compilation phases
    #[arg(long)]
    pub timings: bool,
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
    /// The Zirco TAST, stringified to Zirco code again
    ///
    /// This usually looks like your code with a bunch of parenthesis added.
    #[display("tast")]
    Tast,
    /// Assembly
    #[display("asm")]
    Asm,
    /// Object file
    #[display("object")]
    Object,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frontend_opt_level_converts_to_optimization_level_correctly() {
        assert_eq!(
            OptimizationLevel::from(FrontendOptLevel::O0),
            OptimizationLevel::None
        );
        assert_eq!(
            OptimizationLevel::from(FrontendOptLevel::O1),
            OptimizationLevel::Less
        );
        assert_eq!(
            OptimizationLevel::from(FrontendOptLevel::O2),
            OptimizationLevel::Default
        );
        assert_eq!(
            OptimizationLevel::from(FrontendOptLevel::O3),
            OptimizationLevel::Aggressive
        );
    }

    #[test]
    fn output_format_display_works_correctly() {
        assert_eq!(OutputFormat::Llvm.to_string(), "llvm");
        assert_eq!(OutputFormat::AstDebug.to_string(), "ast-debug");
        assert_eq!(OutputFormat::AstDebugPretty.to_string(), "ast-debug-pretty");
        assert_eq!(OutputFormat::Ast.to_string(), "ast");
        assert_eq!(OutputFormat::TastDebug.to_string(), "tast-debug");
        assert_eq!(
            OutputFormat::TastDebugPretty.to_string(),
            "tast-debug-pretty"
        );
        assert_eq!(OutputFormat::Tast.to_string(), "tast");
        assert_eq!(OutputFormat::Asm.to_string(), "asm");
        assert_eq!(OutputFormat::Object.to_string(), "object");
    }

    #[test]
    fn get_include_paths_resolves_relative_paths_to_absolute() {
        // Create a CLI with a relative path
        let cli = Cli {
            version: false,
            path: None,
            out_file: PathBuf::from("-"),
            emit: OutputFormat::Llvm,
            force: false,
            target: None,
            cpu: String::from("generic"),
            opt_level: FrontendOptLevel::O2,
            debug: false,
            include_paths: vec![PathBuf::from(".")],
            timings: false,
        };

        let paths = get_include_paths(&cli);

        // The relative path "." should be resolved to an absolute path
        assert_eq!(paths.len(), 1);
        assert!(paths[0].is_absolute(), "Path should be absolute");
    }

    #[test]
    fn get_include_paths_preserves_absolute_paths() {
        // Create a CLI with an absolute path
        let absolute_path = PathBuf::from("/tmp");
        let cli = Cli {
            version: false,
            path: None,
            out_file: PathBuf::from("-"),
            emit: OutputFormat::Llvm,
            force: false,
            target: None,
            cpu: String::from("generic"),
            opt_level: FrontendOptLevel::O2,
            debug: false,
            include_paths: vec![absolute_path.clone()],
            timings: false,
        };

        let paths = get_include_paths(&cli);

        // The absolute path should be preserved
        assert_eq!(paths.len(), 1);
        assert!(paths[0].is_absolute(), "Path should be absolute");
        assert_eq!(paths[0], absolute_path.as_path());
    }
}
