//! IO module for driving the reading and writing of Zirco source files
//!
//! This module provides functions to open input and output files,
//! handling both standard input/output (denoted by "-") and regular files.
//! It returns appropriate readers and writers for use in the compilation
//! process.

use std::{
	fs,
	io::{self, Read},
	path::PathBuf,
};

use tracing::{debug, instrument};

/// Opens the input file and returns a reader
///
/// If the path is "-", it reads from standard input.
/// If the path is a regular file, it opens the file for reading.
/// It returns a tuple containing the directory name, file name, and a boxed
/// reader.
///
/// # Errors
///
/// If the file cannot be opened, an error is returned.
///
/// # Panics
///
/// If the file name or directory name cannot be converted to a valid string.
#[instrument]
pub fn open_input(path: &PathBuf) -> Result<Box<dyn Read>, io::Error> {
	if path.as_os_str() == "-" {
		debug!("reading from standard input");
		Ok(Box::new(io::stdin()))
	} else {
		let file = fs::File::open(path)?;
		debug!(path = ?path, "opening input");

		Ok(Box::new(file))
	}
}

/// Opens the output file and returns a writer
///
/// If the path is "-", it writes to standard output.
/// If the path is a regular file, it opens (or creates) the file for writing,
/// truncating it if it already exists.
/// It returns a boxed writer.
///
/// # Errors
///
/// If the file cannot be opened or created, an error is returned.
#[instrument]
pub fn open_output(path: &PathBuf) -> Result<Box<dyn io::Write>, io::Error> {
	Ok(if path.as_os_str() == "-" {
		debug!("writing to standard output");
		Box::new(io::stdout())
	} else {
		debug!(path = ?path, "opening output file");
		Box::new(
			fs::OpenOptions::new()
				.write(true)
				.truncate(true)
				.create(true)
				.open(path)?,
		)
	})
}
