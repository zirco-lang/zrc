# `zrc_preprocessor` - Zirco Preprocessor

This crate implements the preprocessor for the Zirco programming language. The preprocessor handles file inclusion directives before the source code is passed to the lexer and parser.

## Features

- **File Inclusion**: Support for `#include` directives with both `"file"` and `<file>` syntax
- **Circular Include Detection**: Prevents infinite loops from circular file includes
- **Relative Path Resolution**: Includes are resolved relative to the including file
- **Error Reporting**: Clear error messages using the Zirco diagnostic system

## Usage

```rust
use zrc_preprocessor::preprocess;
use std::path::Path;

let source = r#"
#include "helpers.zr"

fn main() {
    return helper_function();
}
"#;

let file_path = Path::new("main.zr");
let preprocessed = preprocess(source, file_path)?;
```

## Include Syntax

The preprocessor supports two forms of include directives:

### Quoted Includes
```zirco
#include "relative/path/to/file.zr"
```

### Angle Bracket Includes
```zirco
#include <library/file.zr>
```

Both syntaxes work identically and resolve paths relative to the current file.

## Implementation Details

The preprocessor:
1. Reads the source file line by line
2. When it encounters an `#include` directive, it:
   - Parses the included file path
   - Resolves it relative to the current file
   - Recursively preprocesses the included file
   - Inserts the preprocessed content in place of the directive
3. Tracks all processed files to prevent circular includes
4. Returns the fully preprocessed source code

## Error Handling

The preprocessor can return the following errors:
- **PreprocessorError**: General preprocessing errors (e.g., path resolution failures)
- **PreprocessorCircularInclude**: Circular include detected
- **PreprocessorInvalidIncludeSyntax**: Invalid `#include` directive syntax
- **PreprocessorFileNotFound**: Included file could not be read
