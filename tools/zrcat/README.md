# zrcat - Zirco C Auto Translator

A tool for translating C header files to Zirco `.zh` equivalents.

## Overview

`zrcat` (Zirco C Auto Translator) automatically converts C header files into Zirco header format. It handles:

- Function declarations
- Struct definitions
- Typedef declarations
- Type conversions (C types → Zirco types)
- Variadic functions
- Pointer types

## Usage

### Basic Usage

```bash
# Translate a C header file
zrcat input.h

# Output to a file
zrcat input.h -o output.zh

# Read from stdin
cat input.h | zrcat --stdin

# Preprocess with cpp first (recommended for system headers)
echo '#include <stdio.h>' | zrcat --stdin -p
```

### Options

- `input` - Input C header file path (optional if using `--stdin`)
- `-o, --output <file>` - Output file path (default: stdout)
- `--stdin` - Read from stdin instead of a file
- `-p, --preprocess` - Run C preprocessor (cpp) on input first
- `-V, --version` - Show version information

## Type Mappings

`zrcat` translates C types to Zirco types as follows:

| C Type | Zirco Type |
|--------|------------|
| `void` | `void` |
| `char` | `i8` |
| `unsigned char` | `u8` |
| `short` | `i16` |
| `unsigned short` | `u16` |
| `int` | `i32` |
| `unsigned int` | `u32` |
| `long` | `i64` |
| `unsigned long` | `u64` |
| `long long` | `i64` |
| `unsigned long long` | `u64` |
| `float` | `f32` |
| `double` | `f64` |
| `size_t` | `usize` |
| `bool` / `_Bool` | `bool` |

Pointers are converted by prefixing with `*` (e.g., `int *` → `*i32`).

## Examples

### Function Declarations

Input (C):
```c
int add(int a, int b);
void hello(void);
int printf(const char* format, ...);
```

Output (Zirco):
```zirco
fn add(a: i32, b: i32) -> i32;
fn hello() -> void;
fn printf(format: *i8, ...) -> i32;
```

### Struct Definitions

Input (C):
```c
struct Point {
    int x;
    int y;
};

typedef struct {
    int width;
    int height;
} Rectangle;
```

Output (Zirco):
```zirco
struct Point {
    x: i32,
    y: i32,
}

struct Rectangle {
    width: i32,
    height: i32,
}
```

## Limitations

- **Preprocessor**: For best results with system headers, use the `-p` flag to preprocess first
- **Complex macros**: Macros are not translated (they're removed during preprocessing)
- **Complex types**: Function pointers and arrays have limited support
- **Comments**: Comments are removed during processing
- **Inline functions**: Function definitions are ignored (only declarations are translated)

## Building from Source

```bash
cd tools/zrcat
cargo build --release
```

The binary will be at `target/release/zrcat`.

## Installation

```bash
cargo install --path tools/zrcat
```

## License

Part of the Zirco project. See the main project LICENSE for details.
