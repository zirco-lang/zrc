//! Advanced example showing multi-span error reporting capabilities
//!
//! This demonstrates how codespan-reporting and ariadne can show multiple
//! related locations in a single diagnostic

use zrc_utils::span::Span;
use zrc_diagnostics::{DiagnosticKind, SpanExt};

#[cfg(feature = "codespan")]
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label, Severity};
#[cfg(feature = "codespan")]
use codespan_reporting::files::SimpleFiles;
#[cfg(feature = "codespan")]
use codespan_reporting::term;
#[cfg(feature = "codespan")]
use codespan_reporting::term::termcolor::Buffer;

#[cfg(feature = "ariadne-fmt")]
use ariadne::{Color, Label as AriadneLabel, Report, ReportKind, Source};

fn main() {
    let source = r#"fn calculate(x: i32) -> i32 {
    return x;
}

fn main() {
    let result = calculate("hello");
}"#;

    // The current formatter can only show one location
    let span = Span::from_positions(94, 101); // "hello"
    let diagnostic = span.error(DiagnosticKind::ExpectedGot {
        expected: "i32".to_string(),
        got: "string".to_string(),
    });
    
    println!("=== CURRENT FORMATTER ===");
    println!("{}", diagnostic.print(source));
    println!();
    
    // But codespan can show both the call site AND the function definition
    #[cfg(feature = "codespan")]
    {
        println!("=== CODESPAN-REPORTING (Multi-span) ===");
        
        let mut files = SimpleFiles::new();
        let file_id = files.add("example.zr", source);
        
        let diagnostic = CodespanDiagnostic::new(Severity::Error)
            .with_message("type mismatch in function call")
            .with_labels(vec![
                Label::primary(file_id, 94..101)
                    .with_message("expected `i32`, found `string`"),
                Label::secondary(file_id, 13..16)
                    .with_message("function expects `i32` here"),
            ])
            .with_notes(vec![
                "function `calculate` expects an integer argument".to_string(),
            ]);
        
        let mut buffer = Buffer::ansi();
        let config = term::Config::default();
        term::emit(&mut buffer, &config, &files, &diagnostic)
            .expect("failed to emit diagnostic");
        println!("{}", String::from_utf8_lossy(buffer.as_slice()));
    }
    
    // Ariadne can also show multiple spans
    #[cfg(feature = "ariadne-fmt")]
    {
        println!("=== ARIADNE (Multi-span) ===");
        
        let report = Report::build(ReportKind::Error, "example.zr", 94)
            .with_message("type mismatch in function call")
            .with_label(
                AriadneLabel::new(("example.zr", 94..101))
                    .with_message("expected `i32`, found `string`")
                    .with_color(Color::Red)
            )
            .with_label(
                AriadneLabel::new(("example.zr", 13..16))
                    .with_message("function expects `i32` here")
                    .with_color(Color::Blue)
            )
            .with_note("function `calculate` expects an integer argument")
            .finish();
        
        let mut buffer = Vec::new();
        report
            .write(("example.zr", Source::from(source)), &mut buffer)
            .expect("failed to write diagnostic");
        println!("{}", String::from_utf8_lossy(&buffer));
    }
    
    // Another example: conflicting declarations
    let source2 = r#"let x: i32 = 5;
let x: string = "hello";"#;
    
    let span2 = Span::from_positions(16, 18);
    let diagnostic2 = span2.error(DiagnosticKind::IdentifierAlreadyInUse("x".to_string()));
    
    println!("=== CURRENT FORMATTER (Duplicate Declaration) ===");
    println!("{}", diagnostic2.print(source2));
    println!();
    
    #[cfg(feature = "codespan")]
    {
        println!("=== CODESPAN-REPORTING (Duplicate Declaration) ===");
        
        let mut files = SimpleFiles::new();
        let file_id = files.add("example.zr", source2);
        
        let diagnostic = CodespanDiagnostic::new(Severity::Error)
            .with_message("identifier `x` is already in use")
            .with_labels(vec![
                Label::primary(file_id, 20..21)
                    .with_message("second declaration of `x`"),
                Label::secondary(file_id, 4..5)
                    .with_message("first declared here"),
            ])
            .with_notes(vec![
                "consider using a different name or removing one of the declarations".to_string(),
            ]);
        
        let mut buffer = Buffer::ansi();
        let config = term::Config::default();
        term::emit(&mut buffer, &config, &files, &diagnostic)
            .expect("failed to emit diagnostic");
        println!("{}", String::from_utf8_lossy(buffer.as_slice()));
    }
    
    #[cfg(feature = "ariadne-fmt")]
    {
        println!("=== ARIADNE (Duplicate Declaration) ===");
        
        let report = Report::build(ReportKind::Error, "example.zr", 20)
            .with_message("identifier `x` is already in use")
            .with_label(
                AriadneLabel::new(("example.zr", 20..21))
                    .with_message("second declaration of `x`")
                    .with_color(Color::Red)
            )
            .with_label(
                AriadneLabel::new(("example.zr", 4..5))
                    .with_message("first declared here")
                    .with_color(Color::Blue)
            )
            .with_help("consider using a different name or removing one of the declarations")
            .finish();
        
        let mut buffer = Vec::new();
        report
            .write(("example.zr", Source::from(source2)), &mut buffer)
            .expect("failed to write diagnostic");
        println!("{}", String::from_utf8_lossy(&buffer));
    }
}
