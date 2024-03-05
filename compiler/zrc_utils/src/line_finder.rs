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
    /// The spans of each line in the input
    line_spans: Vec<(usize, usize)>,
}

impl LineLookup {
    /// Creates a new [`CgLineLookup`] over a string
    #[must_use]
    pub fn new(input: &str) -> Self {
        let mut line_start = 0;
        let mut line_spans = vec![];
        for line in input.split('\n') {
            let line_end = line_start + line.len() + "\n".len();
            // TODO: this assumes lines terminate with \n, not \r\n.
            line_spans.push((line_start, line_end - 1));
            line_start = line_end;
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
            .binary_search_by(|(line_start, line_end)| {
                if *line_end < index {
                    return Ordering::Less;
                }
                if *line_start > index {
                    return Ordering::Greater;
                }

                Ordering::Equal
            })
            .expect("line should be present");

        LineAndCol {
            line: u32::try_from(line)
                .expect("there should probably never be more than u32::MAX lines in a file")
                + 1,
            col: u32::try_from(index - self.line_spans[line].0)
                .expect("there should probably never be more than u32::MAX columns in a line")
                + 1,
        }
    }
}
