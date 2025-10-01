//! IO module for driving the reading and writing of Zirco source files

use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

/// Opens the input file and returns a reader
pub fn open_input(path: &PathBuf) -> anyhow::Result<(String, String, Box<dyn Read>)> {
    if path.as_os_str() == "-" {
        Ok((
            "/dev".to_string(),
            "stdin".to_string(),
            Box::new(io::stdin()),
        ))
    } else {
        let file = fs::File::open(path)?;
        let mut canonical = fs::canonicalize(path)?;
        let file_name = canonical
            .file_name()
            .expect("file name should exist")
            .to_str()
            .expect("should be a valid str")
            .to_string();
        canonical.pop();
        let directory_name = canonical
            .to_str()
            .expect("directory should be a valid str")
            .to_string();

        Ok((directory_name, file_name, Box::new(file)))
    }
}

/// Opens the output file and returns a writer
pub fn open_output(path: &PathBuf) -> anyhow::Result<Box<dyn io::Write>> {
    Ok(if path.as_os_str() == "-" {
        Box::new(io::stdout())
    } else {
        Box::new(
            fs::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(path)?,
        )
    })
}
