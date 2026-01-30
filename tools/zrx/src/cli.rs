//! Command line interface declarations for the zrx JIT

use std::{
    env,
    path::{Path, PathBuf},
};

use clap::Parser;
use zrc_codegen::OptimizationLevel;

/// The Zirco JIT execution CLI
#[derive(Parser)]
#[command(version=None)]
pub struct Cli {
    /// See what version of zrx you are using
    #[arg(short, long)]
    pub version: bool,

    /// The main file to build and execute
    pub path: Option<PathBuf>,

    /// Additional files to include in the build
    #[arg(short = 'f', long = "file", action = clap::ArgAction::Append)]
    pub extra_files: Vec<PathBuf>,

    /// Set the optimization level
    #[arg(short = 'O', long = "opt-level")]
    #[clap(default_value = "default")]
    pub opt_level: FrontendOptLevel,

    /// Add a directory to the include path
    #[arg(short = 'I', long = "include", action = clap::ArgAction::Append)]
    pub include_paths: Vec<PathBuf>,

    /// Add a directory to the library search path
    #[arg(short = 'L', long = "lib-path", action = clap::ArgAction::Append)]
    pub lib_paths: Vec<PathBuf>,

    /// Include a library to link against
    #[arg(short = 'l', long = "library", action = clap::ArgAction::Append)]
    pub libraries: Vec<String>,

    /// Trailing arguments to pass to the executed program
    #[arg(trailing_var_arg = true)]
    pub program_args: Vec<String>,
}

/// Configuration for the Zirco optimizer
#[derive(Debug, Clone, Copy, clap::ValueEnum, PartialEq, Eq)]
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

/// Resolve a path to an absolute path based on the current working directory.
///
/// If the path is relative, it is resolved to an absolute path by joining it
/// with the current working directory and canonicalizing it. If the path is
/// already absolute or canonicalization fails, the path is returned as-is.
fn resolve_include_path(path: &Path) -> PathBuf {
    if path.is_relative() {
        env::current_dir()
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

    if let Ok(env_paths) = env::var("ZIRCO_INCLUDE_PATH") {
        for path_str in env::split_paths(&env_paths) {
            let resolved_path = resolve_include_path(&path_str);

            // SAFETY: we leak the PathBuf to get a 'static lifetime
            let static_path: &'static Path = Box::leak(resolved_path.into_boxed_path());
            include_paths.push(static_path);
        }
    }

    include_paths
}
