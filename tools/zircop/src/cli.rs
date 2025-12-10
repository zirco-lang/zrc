//! Command line interface declarations for the Zircop linter

use std::{
    env,
    path::{Path, PathBuf},
};

use clap::Parser;

/// The official Zirco linter
#[derive(Parser)]
#[command(version=None)]
pub struct Cli {
    /// See what version of zircop you are using
    #[arg(short, long)]
    pub version: bool,

    /// The path of the file to lint
    pub path: Option<PathBuf>,

    /// Add a directory to the include path
    #[arg(short = 'I', long = "include", action = clap::ArgAction::Append)]
    pub include_paths: Vec<PathBuf>,
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
        let static_path: &'static Path = Box::leak(resolved_path.clone().into_boxed_path());
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
