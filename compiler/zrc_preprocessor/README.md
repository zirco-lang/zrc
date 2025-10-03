# zrc_preprocessor

The Zirco preprocessor provides basic C-like preprocessing capabilities, primarily supporting `#include` directives to combine multiple source files into a single compilation unit.

## Features

- **`#include "filename"`** - Include other Zirco source files
- **Circular include detection** - Prevents infinite recursion
- **File tracking** - Maintains source file information for diagnostics
- **Line preservation** - Replaces directives with blank lines to maintain line numbers

## Usage

The preprocessor is automatically used by the compiler when `#include` directives are detected in the source code.

### Example

**utils.zrc:**
```zirco
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

**main.zrc:**
```zirco
#include "utils.zrc"

fn main() -> i32 {
    return add(2, 3);
}
```

The preprocessor combines these files into a single compilation unit while tracking which declarations came from which file for accurate error reporting.

## API

The main entry point is the `preprocess` function:

```rust
pub fn preprocess(
    file_path: &Path,
    parent_dir: &Path,
) -> Result<Vec<Spanned<Declaration<'static>>>, Diagnostic>
```

This function:
1. Reads the source file
2. Extracts `#include` directives
3. Recursively processes included files
4. Combines all declarations into a single list
5. Returns the combined AST with file tracking information

## Limitations

- Only supports `#include "filename"` syntax (no `<>` includes)
- Include paths are relative to the parent directory
- No macro expansion or conditional compilation
- Circular includes are silently skipped (first occurrence wins)
