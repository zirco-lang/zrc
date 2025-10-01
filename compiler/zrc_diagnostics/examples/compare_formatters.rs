//! Example program to compare different error formatting outputs
//!
//! This demonstrates the current formatter vs codespan-reporting vs ariadne

use zrc_utils::span::Span;
use zrc_diagnostics::{DiagnosticKind, SpanExt};

fn main() {
    let source = r#"fn main() {
    let x = 5;
    let y = unknown_var;
    println("Hello {}", x);
}"#;

    // Create a diagnostic for an unknown identifier
    let span = Span::from_positions(37, 48); // "unknown_var"
    let diagnostic = span.error(DiagnosticKind::UnableToResolveIdentifier("unknown_var".to_string()));
    
    println!("=== CURRENT FORMATTER ===");
    println!("{}", diagnostic.print(source));
    println!();
    
    #[cfg(feature = "codespan")]
    {
        println!("=== CODESPAN-REPORTING FORMATTER ===");
        println!("{}", diagnostic.print_with_codespan(source, "example.zr"));
        println!();
    }
    
    #[cfg(feature = "ariadne-fmt")]
    {
        println!("=== ARIADNE FORMATTER ===");
        println!("{}", diagnostic.print_with_ariadne(source, "example.zr"));
        println!();
    }

    // Example with type mismatch error
    let span2 = Span::from_positions(67, 68); // "x" in println
    let diagnostic2 = span2.error(DiagnosticKind::ExpectedGot {
        expected: "string".to_string(),
        got: "i32".to_string(),
    });
    
    println!("=== CURRENT FORMATTER (Type Error) ===");
    println!("{}", diagnostic2.print(source));
    println!();
    
    #[cfg(feature = "codespan")]
    {
        println!("=== CODESPAN-FORMATTING FORMATTER (Type Error) ===");
        println!("{}", diagnostic2.print_with_codespan(source, "example.zr"));
        println!();
    }
    
    #[cfg(feature = "ariadne-fmt")]
    {
        println!("=== ARIADNE FORMATTER (Type Error) ===");
        println!("{}", diagnostic2.print_with_ariadne(source, "example.zr"));
        println!();
    }
}
