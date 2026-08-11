//! Utilities for the Zirco JIT

use std::{env, path::PathBuf};

/// Split an environment variable containing paths into a vector of [`PathBuf`]s
pub fn split_paths(var: &str) -> Vec<PathBuf> {
	env::var_os(var)
		.map(|val| env::split_paths(&val).collect())
		.unwrap_or_default()
}

/// Get the possible library filenames for a given library name on this platform
pub fn library_filenames(name: &str) -> Vec<String> {
	#[cfg(target_os = "linux")]
	{
		vec![format!("lib{name}.so")]
	}

	#[cfg(target_os = "macos")]
	{
		vec![format!("lib{name}.dylib")]
	}

	#[cfg(target_os = "windows")]
	{
		vec![format!("{name}.dll")]
	}
}

/// Resolve a library name to a full path by searching in the given search paths
pub fn resolve_library(name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
	let candidates = library_filenames(name);

	for dir in search_paths {
		for file in &candidates {
			let path = dir.join(file);
			if path.exists() {
				return Some(path);
			}
		}
	}

	None
}
