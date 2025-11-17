# Zirco TextMate Grammar

This directory contains the TextMate grammar definition for the Zirco programming language.

## File

- `zirco.tmLanguage.json` - TextMate grammar in JSON format

## Supported Features

The grammar provides syntax highlighting for:

### Language Constructs
- **Keywords**: `if`, `else`, `while`, `do`, `for`, `four`, `break`, `continue`, `return`, `switch`, `match`, `default`, `unreachable`
- **Declaration keywords**: `fn`, `let`, `type`, `struct`, `union`, `as`, `sizeof`
- **Primitive types**: `i8`, `i16`, `i32`, `i64`, `isize`, `u8`, `u16`, `u32`, `u64`, `usize`, `bool`
- **Boolean literals**: `true`, `false`

### Preprocessor Directives
- `#include "file.zr"` and `#include <file.zr>`
- `#pragma once`

### Literals
- **Numbers**: Decimal (`42`, `1_000`), hexadecimal (`0xFF`, `0xDEAD_BEEF`), binary (`0b1010`)
- **Type suffixes**: `42i32`, `100u64`, etc.
- **Strings**: Double-quoted with escape sequences (`\n`, `\r`, `\t`, `\\`, `\"`)
- **Characters**: Single-quoted with escape sequences (`'a'`, `'\n'`)

### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `++`, `--`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `&&`, `||`, `!`
- **Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`
- **Assignment**: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- **Other**: `.`, `->`, `::`, `?`, `:`, `=>`
- **Pointer**: `*` (dereference), `&` (address-of)

### Declarations
- Function declarations with parameter highlighting
- Type alias declarations
- Struct declarations
- Union declarations
- Variadic functions (`...`)

### Comments
- Single-line comments (`//`)

## Usage

### Visual Studio Code

To use this grammar in VS Code, you can create a VS Code extension or add it to your workspace settings:

1. Copy `zirco.tmLanguage.json` to your VS Code extensions directory
2. Create a `package.json` for the extension
3. The grammar will automatically be applied to `.zr` and `.zh` files

### GitHub

GitHub automatically recognizes TextMate grammars placed in the `syntaxes/` directory of a repository, providing syntax highlighting for Zirco files in the web interface.

### Sublime Text

Sublime Text can use TextMate grammars directly:

1. Place the grammar file in `Packages/User/`
2. The grammar will be available for `.zr` and `.zh` files

## File Extensions

The grammar is configured to work with:
- `.zr` - Zirco source files
- `.zh` - Zirco header files

## Scope Names

The grammar uses the scope name `source.zirco` as the root scope. Common scopes include:

- `comment.line.double-slash.zirco` - Comments
- `keyword.control.zirco` - Control flow keywords
- `keyword.other.zirco` - Other keywords
- `storage.type.zirco` - Type names
- `entity.name.function.zirco` - Function names
- `entity.name.type.zirco` - Custom type names
- `constant.numeric.*.zirco` - Numeric literals
- `string.quoted.*.zirco` - String and character literals

## Contributing

To improve the grammar:

1. Edit `zirco.tmLanguage.json`
2. Test with various Zirco source files
3. Ensure all language features are properly highlighted

## References

- [Zirco Language Specification](../docs/SPEC.md)
- [TextMate Grammars Documentation](https://macromates.com/manual/en/language_grammars)
- [VS Code Syntax Highlighting Guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
