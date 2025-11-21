//! Command line interface declarations for the Zircop linter

use std::{
    env,
    path::{Path, PathBuf},
};

use clap::Parser;

/// The official Zirco linter
#[derive(Parser)]
#[command(version, short_flag = 'v')]
pub struct Cli {
    /// The path of the file to lint
    pub path: Option<PathBuf>,

    /// Add a directory to the include path
    #[arg(short = 'I', long = "include", action = clap::ArgAction::Append)]
    pub include_paths: Vec<PathBuf>,
}

/// Get the include paths from the CLI environment and -I arguments
pub fn get_include_paths(cli: &Cli) -> Vec<&'static Path> {
    // append paths in the following order:
    // 1. CLI
    // 2. ZIRCO_INCLUDE_PATH env var
    let mut include_paths: Vec<&'static Path> = Vec::new();

    for path in &cli.include_paths {
        // SAFETY: we leak the PathBuf to get a 'static lifetime
        let static_path: &'static Path = Box::leak(path.clone().into_boxed_path());
        include_paths.push(static_path);
    }

    if let Ok(env_paths) = env::var("ZIRCO_INCLUDE_PATH") {
        for path_str in env::split_paths(&env_paths) {
            // SAFETY: we leak the PathBuf to get a 'static lifetime
            let static_path: &'static Path = Box::leak(path_str.into_boxed_path());
            include_paths.push(static_path);
        }
    }

    include_paths
}
