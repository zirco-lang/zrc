# diagnostics for the zirco compiler

This crate provides error diagnostic and formatting capabilities for the Zirco compiler.

## Features

The crate includes:
- Custom error diagnostic types (`Diagnostic`, `DiagnosticKind`)
- Span-based error reporting
- Colorful, formatted error output

### Optional Error Formatters

Two alternative error formatting libraries have been investigated and integrated as optional features:

- **`codespan-reporting`**: Professional error messages with multi-span support, notes, and extensive customization
- **`ariadne`**: Beautiful, Rust-inspired error messages with compact, visually appealing output

Enable these features in your `Cargo.toml`:

```toml
[dependencies]
zrc_diagnostics = { version = "0.1.0", features = ["codespan"] }
# or
zrc_diagnostics = { version = "0.1.0", features = ["ariadne-fmt"] }
```

See [ERROR_FORMATTING_INVESTIGATION.md](./ERROR_FORMATTING_INVESTIGATION.md) for a detailed comparison and recommendations.

## Examples

Run the example programs to see the different formatters in action:

```bash
# Compare all three formatters
cargo run --example compare_formatters --all-features

# See advanced multi-span capabilities
cargo run --example advanced_formatting --all-features
```

## Usage

Current formatter (default):
```rust
use zrc_diagnostics::{DiagnosticKind, SpanExt};
use zrc_utils::span::Span;

let span = Span::from_positions(0, 5);
let diagnostic = span.error(DiagnosticKind::UnknownToken("foo".to_string()));
println!("{}", diagnostic.print(source));
```

With codespan-reporting (requires `codespan` feature):
```rust
println!("{}", diagnostic.print_with_codespan(source, "file.zr"));
```

With ariadne (requires `ariadne-fmt` feature):
```rust
println!("{}", diagnostic.print_with_ariadne(source, "file.zr"));
```
