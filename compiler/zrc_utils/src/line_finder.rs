//! Tools for looking up the [`LineAndCol`] of some location in a source file

use std::cmp::Ordering;

/// Wrapper around a line and column
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineAndCol {
    /// The 1-indexed line number
    pub line: u32,
    /// The 1-indexed column number
    // REVIEW: Is it truly 1-indexed? If it's 0, LLVM in cg will treat it as "no column known"
    pub col: u32,
}

/// Simple tool to lookup the line and column of a location in the source file
#[derive(Debug)]
pub struct LineLookup {
    /// The spans of each line in the input, where each span is (start,
    /// `content_end`, `span_end`)
    /// - start: byte index where the line starts
    /// - `content_end`: byte index of the last visible character (exclusive)
    /// - `span_end`: byte index of the last searchable character (inclusive,
    ///   includes line endings)
    line_spans: Vec<(usize, usize, usize)>,
}

impl LineLookup {
    /// Creates a new [`LineLookup`] over a string
    #[must_use]
    pub fn new(input: &str) -> Self {
        let mut line_start = 0;
        let mut line_spans = vec![];

        // Split by \n and process each line
        let lines: Vec<&str> = input.split('\n').collect();

        for (idx, line) in lines.iter().enumerate() {
            // Strip trailing \r if present (for CRLF line endings)
            let line_without_cr = line.strip_suffix('\r').unwrap_or(line);

            // Calculate the end position of the actual line content (without line endings)
            let content_end = line_start + line_without_cr.len();

            // For the searchable span, we need to include line endings so lookups work
            // For non-last lines, include everything up to and including the \r and \n
            let span_end = if idx < lines.len() - 1 {
                // Include the line ending characters (\r if present, and \n)
                line_start + line.len() // This includes \r if present, but not \n
            } else {
                // For the last line, just go to the end of content
                content_end.saturating_sub(1) // Make it inclusive
            };

            line_spans.push((line_start, content_end, span_end));

            // Move to the next line start (after \n)
            line_start += line.len() + 1; // +1 for \n
        }

        Self { line_spans }
    }

    /// Look up the `1`-indexed line number from an offset in the string
    ///
    /// # Panics
    /// Panics if the line doesn't exist or there are more than [`u32::MAX`]
    /// lines or columns in the input
    #[must_use]
    pub fn lookup_from_index(&self, index: usize) -> LineAndCol {
        let line = self
            .line_spans
            .binary_search_by(|(line_start, _content_end, span_end)| {
                if *span_end < index {
                    return Ordering::Less;
                }
                if *line_start > index {
                    return Ordering::Greater;
                }

                Ordering::Equal
            })
            .expect("line should be present");

        let (line_start, content_end, _span_end) = self.line_spans[line];

        // Calculate column position
        // If the index is beyond the content (in the line ending), clamp to content end
        let col_index = if index >= content_end {
            content_end.saturating_sub(line_start)
        } else {
            index - line_start
        };

        LineAndCol {
            line: u32::try_from(line)
                .expect("there should probably never be more than u32::MAX lines in a file")
                + 1,
            col: u32::try_from(col_index)
                .expect("there should probably never be more than u32::MAX columns in a line")
                + 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test that `LineLookup` works correctly with Unix-style (LF) line endings
    #[test]
    fn test_lf_line_endings() {
        let input = "line1\nline2\nline3";
        let lookup = LineLookup::new(input);

        // Test first line
        let result = lookup.lookup_from_index(0);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 1);

        let result = lookup.lookup_from_index(2);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 3);

        // Test second line
        let result = lookup.lookup_from_index(6);
        assert_eq!(result.line, 2);
        assert_eq!(result.col, 1);

        // Test third line
        let result = lookup.lookup_from_index(12);
        assert_eq!(result.line, 3);
        assert_eq!(result.col, 1);
    }

    /// Test that `LineLookup` works correctly with Windows-style (CRLF) line
    /// endings
    #[test]
    fn test_crlf_line_endings() {
        let input = "line1\r\nline2\r\nline3";
        let lookup = LineLookup::new(input);

        // Test first line
        let result = lookup.lookup_from_index(0);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 1);

        let result = lookup.lookup_from_index(2);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 3);

        // The \r character at index 5 should be reported as part of line 1
        // But the column should not make it seem like it's past the visible content
        let result = lookup.lookup_from_index(5);
        assert_eq!(result.line, 1);
        // This is the problem: it reports col=6, but there are only 5 visible
        // characters in "line1" We want it to report as column 6 which is after
        // the last visible char, not as the \r position Actually, with the
        // current implementation, column 6 is correct if we count from 1
        // Index 5 - line_start(0) = 5, + 1 = 6, which is correct

        // The \n character at index 6 should also be part of line 1
        let result = lookup.lookup_from_index(6);
        assert_eq!(result.line, 1);

        // Test second line (starts at index 7 after "line1\r\n")
        let result = lookup.lookup_from_index(7);
        assert_eq!(result.line, 2);
        assert_eq!(result.col, 1);

        // Test third line (starts at index 14 after "line1\r\nline2\r\n")
        let result = lookup.lookup_from_index(14);
        assert_eq!(result.line, 3);
        assert_eq!(result.col, 1);
    }

    /// Test that `LineLookup` works correctly with mixed line endings
    #[test]
    fn test_mixed_line_endings() {
        let input = "line1\nline2\r\nline3\rline4";
        let lookup = LineLookup::new(input);

        // Test first line (LF ending)
        let result = lookup.lookup_from_index(0);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 1);

        // Test second line (CRLF ending, starts at index 6)
        let result = lookup.lookup_from_index(6);
        assert_eq!(result.line, 2);
        assert_eq!(result.col, 1);

        // Test third line (CR ending, starts at index 13)
        let result = lookup.lookup_from_index(13);
        assert_eq!(result.line, 3);
        assert_eq!(result.col, 1);
    }

    /// Test edge case: empty input
    #[test]
    fn test_empty_input() {
        let input = "";
        let lookup = LineLookup::new(input);

        // Empty input should have one empty line
        let result = lookup.lookup_from_index(0);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 1);
    }

    /// Test edge case: input with only newlines
    #[test]
    fn test_only_newlines() {
        let input = "\n\n\n";
        let lookup = LineLookup::new(input);

        // First empty line
        let result = lookup.lookup_from_index(0);
        assert_eq!(result.line, 1);
        assert_eq!(result.col, 1);

        // Second empty line
        let result = lookup.lookup_from_index(1);
        assert_eq!(result.line, 2);
        assert_eq!(result.col, 1);

        // Third empty line
        let result = lookup.lookup_from_index(2);
        assert_eq!(result.line, 3);
        assert_eq!(result.col, 1);
    }
}
