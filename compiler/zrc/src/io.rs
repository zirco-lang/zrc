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

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;

    #[test]
    fn open_input_reads_from_stdin_for_dash() {
        let path = PathBuf::from("-");
        let result = open_input(&path);
        assert!(result.is_ok());
        let (dir, file, _reader) = result.expect("should succeed");
        assert_eq!(dir, "/dev");
        assert_eq!(file, "stdin");
    }

    #[test]
    fn open_input_reads_from_file() {
        let temp_file = std::env::temp_dir().join("test_input.zrc");
        fs::write(&temp_file, "test content").expect("should write test file");

        let result = open_input(&temp_file);
        assert!(result.is_ok());

        let (dir, file, mut reader) = result.expect("should succeed");
        assert_eq!(file, "test_input.zrc");
        assert!(!dir.is_empty());

        let mut content = String::new();
        reader.read_to_string(&mut content).expect("should read");
        assert_eq!(content, "test content");

        fs::remove_file(&temp_file).expect("should cleanup test file");
    }

    #[test]
    fn open_output_writes_to_stdout_for_dash() {
        let path = PathBuf::from("-");
        let result = open_output(&path);
        assert!(result.is_ok());
    }

    #[test]
    fn open_output_creates_file() {
        let temp_file = std::env::temp_dir().join("test_output.zrc");

        let result = open_output(&temp_file);
        assert!(result.is_ok());

        let mut writer = result.expect("should succeed");
        writer.write_all(b"test output").expect("should write");
        drop(writer);

        let content = fs::read_to_string(&temp_file).expect("should read file");
        assert_eq!(content, "test output");

        fs::remove_file(&temp_file).expect("should cleanup test file");
    }
}
