// This example demonstrates basic preprocessor usage

use std::path::Path;
use zrc_preprocessor::preprocess;

fn main() {
    // Example source code with an include directive
    let source = r#"
#include "math_helpers.zr"

fn main() -> i32 {
    return add(5, 7);
}
"#;

    // Simulate the included file content
    // In a real scenario, this would be read from a file
    let included_content = r#"
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
"#;

    // For this example, we'll just show what the preprocessor would do
    println!("Original source:");
    println!("{}", source);
    println!("\nIncluded file content:");
    println!("{}", included_content);
    println!("\nAfter preprocessing, the source would contain both:");
    println!("{}", included_content);
    println!("{}", source.replace("#include \"math_helpers.zr\"\n", ""));

    // Note: In actual usage, you would:
    // 1. Create actual files
    // 2. Use preprocess() with a real file path
    // 3. The preprocessor will automatically read and include the referenced files
}
