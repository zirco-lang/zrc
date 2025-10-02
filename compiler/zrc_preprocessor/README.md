# `zrc_preprocessor`

The Zirco preprocessor handles file inclusion via `#include` directives.

## Overview

Unlike traditional C preprocessors that work at the text level, this preprocessor
works at the AST level. Each source file is parsed separately, and the resulting
ASTs are combined. This approach:

- Preserves accurate source location information for error messages
- Simplifies implementation
- Avoids the complexity of text-level preprocessing
- Makes the compiler more maintainable

## Features

- `#include "file.zr"` - Include other Zirco source files
- `#pragma once` - Prevent multiple inclusions of the same file
- File path resolution relative to the including file
- Circular include detection
- Comprehensive error messages for missing or malformed includes
- Support for nested includes (includes within included files)

## Usage

```rust
use zrc_preprocessor::preprocess::preprocess;

let content = r#"
fn main() -> i32 {
    return 0;
}
"#;

let result = preprocess("main.zr", content);
assert!(result.is_ok());
let preprocessed = result.unwrap();

// preprocessed.main_file contains the main file without include directives
// preprocessed.included_files contains all transitively included files
assert!(preprocessed.included_files.is_empty());
```

## Design Decisions

### AST-Level Preprocessing

The preprocessor collects all source files (main file + includes) and returns
them separately. The compiler then parses each file individually and combines
the resulting ASTs. This is different from traditional C-style preprocessing
which operates on text before parsing.

**Advantages:**
- Each file's AST has correct spans relative to that file
- Error messages can reference the correct source file
- No need for complex line marker directives
- Simpler implementation

**Trade-offs:**
- Include directives are removed from the main file's content
- All files are parsed in dependency order (includes first, then main file)

### Include Semantics

- Files are included in the order they are encountered
- Each file is included only once (duplicate includes are ignored)
- Files with `#pragma once` are automatically guarded against multiple inclusion
- Circular includes are detected and reported as errors
- Paths are resolved relative to the including file's directory

### `#pragma once`

The `#pragma once` directive provides a simpler alternative to traditional include guards.
When a file contains `#pragma once`, it will only be included once even if it's referenced
multiple times:

```zirco
// utils.zr
#pragma once

fn helper() -> i32 {
    return 42;
}
```

```zirco
// main.zr
#include "utils.zr"
#include "utils.zr"  // This is ignored due to #pragma once

fn main() -> i32 {
    return helper();  // helper() is defined only once
}
```

