# Preprocessor Implementation Summary

This document summarizes the preprocessor feature added to the Zirco compiler.

## Overview

We have implemented a custom preprocessor for the Zirco programming language that handles file inclusion directives, avoiding the need for external tools like the C preprocessor (`cpp`).

## Implementation Details

### New Components

1. **`zrc_preprocessor` crate** - A new compiler crate that implements the preprocessor functionality
   - Location: `compiler/zrc_preprocessor/`
   - Dependencies: `zrc_diagnostics`, `zrc_utils`

2. **Diagnostic error types** - Four new error variants added to `zrc_diagnostics`:
   - `PreprocessorError` - General preprocessing errors
   - `PreprocessorCircularInclude` - Circular include detection
   - `PreprocessorInvalidIncludeSyntax` - Invalid syntax in include directives
   - `PreprocessorFileNotFound` - Missing included files

### Features

1. **File Inclusion Support**
   - Supports both `#include "file"` and `#include <file>` syntax
   - Relative path resolution based on the including file's location
   - Recursive preprocessing of included files

2. **Circular Include Protection**
   - Tracks all processed files using a HashSet
   - Detects and reports circular includes with clear error messages

3. **Error Handling**
   - Integration with Zirco's diagnostic system
   - Clear error messages with file paths and context

### Integration

The preprocessor is integrated into the compilation pipeline in `compiler/zrc/src/main.rs`:
1. The preprocessor runs **before** the parser
2. It receives the file content and path
3. It returns preprocessed content with all includes resolved
4. The preprocessed content is then passed to the parser

## Usage Example

```zirco
// helper.zr
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

// main.zr
#include "helper.zr"

fn main() -> i32 {
    return add(2, 3);
}
```

After preprocessing, the content passed to the parser will be:

```zirco
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn main() -> i32 {
    return add(2, 3);
}
```

## Testing

The implementation includes comprehensive tests:

1. **Unit Tests** (in `compiler/zrc_preprocessor/src/lib.rs`)
   - Test preprocessing without includes
   - Test file inclusion and combination
   - Test circular include detection
   - Test missing file errors
   - Test both quote and angle bracket syntax

2. **Integration Test** (in `compiler/zrc/tests/preprocessor_integration.rs`)
   - Tests the preprocessor with the full compilation pipeline

All tests pass successfully.

## Benefits

1. **No External Dependencies** - No need for `cpp` or other external preprocessors
2. **Better Error Messages** - Integration with Zirco's diagnostic system provides clear, contextual errors
3. **Language-Specific** - Can be extended in the future with Zirco-specific preprocessor directives
4. **Safe** - Prevents circular includes and provides clear error messages

## Future Enhancements

Potential future enhancements could include:
- Macro definitions and expansion (`#define`)
- Conditional compilation (`#if`, `#ifdef`, `#ifndef`)
- Standard library include paths
- Include guards
- Pragma directives

## Files Modified/Added

- `Cargo.toml` - Added preprocessor crate to workspace
- `compiler/zrc/Cargo.toml` - Added preprocessor dependency
- `compiler/zrc/src/main.rs` - Integrated preprocessor into compilation pipeline
- `compiler/zrc_diagnostics/src/lib.rs` - Added preprocessor error variants
- `compiler/zrc_preprocessor/` - New crate (all files)

## Minimal Changes

This implementation follows the principle of minimal changes:
- Only added new code, no existing functionality was modified
- The preprocessor is a clean, separate module
- Integration is a single function call in the compilation pipeline
- All existing tests continue to pass
