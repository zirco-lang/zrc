//! Defines formatting for Zirco compile time diagnostics.

use ansi_term::Color;
use line_span::LineSpanExt;
use zrc_utils::span::Span;

use crate::Severity;

/// Format and display the 'source window' -- the lines of span within str with
/// the underline where the span lies.
pub fn display_source_window(severity: &Severity, span: Span, source: &str) -> String {
    // First, we need to reduce source into only the lines we need to display. A
    // line should be displayed if *the line's span* intersects (see
    // MaybeIntersecting and intersect_spans) with the span we are trying to
    // display.

    // We can do this by iterating over the lines of source, and checking if the
    // line's span intersects with the span we are trying to display. If it does, we
    // add the line to a vector of lines to display.
    let lines = source
        .line_spans()
        .enumerate()
        .filter(|(_, line)| {
            Span::intersect(
                Span::from_positions(line.start(), line.ending()),
                Span::from_positions(span.start(), span.end()),
            )
            .is_some()
        })
        .map(|(n, line)| (n + 1, line))
        // lines now represents all of the lines we will eventually be formatting into the output.
        // we now need to find the span *within the line* that the span intersects with
        // we map each line to its string and intersecting span
        .map(|(n, line)| {
            (n, &source[line.start()..line.end()], {
                let intersection = Span::intersect(
                    Span::from_positions(line.start(), line.ending()),
                    Span::from_positions(span.start(), span.end()),
                )
                .expect("line span should intersect with span");
                (
                    intersection.start() - line.start(),
                    intersection.end() - line.start(),
                )
            })
        })
        .collect::<Vec<_>>();

    // Alright, cool. We now have an iterator over (line number, string, span within
    // string) which can be used to build our display. How much padding goes on
    // each line number?
    let max_line_number_length = lines
        .iter()
        .map(|(line, _, _)| line.to_string().len())
        .max()
        .expect("lines should not be empty")
        + 1; // i like the look of one extra character padding

    // Display format:
    // line | CODE CODE CODE CODE
    //      |      ^^^^
    // For every line in our input, we can generate both sides of it.
    lines
        .into_iter()
        .map(|(line_number, string, (start, end))| {
            format!(
                "{} {string}\n{} {} {}",
                Color::Blue
                    .bold()
                    .paint(format!("{line_number: >max_line_number_length$} |")),
                " ".repeat(max_line_number_length),
                Color::Blue.bold().paint("|"),
                severity
                    .style()
                    .paint(format!("{}{}", " ".repeat(start), "^".repeat(end - start)))
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}
