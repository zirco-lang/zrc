# String Escaping Example

This example demonstrates the escape-character syntax in Zirco.

## Features demonstrated

The example shows:

1. **Newline**:
`\n` is transformed into a newline character (0x0A)

2. **Hexadecimal escapes**:
`\x1b` is transformed into ASCII `ESC`
`\x41` is transformed into ASCII `A`

3. **Unicode Escapes**:
`\u{2400}` is transformed into ␀ (SYMBOL_FOR_NULL)

## Syntax

```zirco
// Newline
"\n";

// Carriage Return
"\r";

// Tab
"\t";

// Null byte
"\0";

// Hexadecimal escape for "A"
"\x41";

// Unicode escape for null byte
"\u{0}";

// Unicode escape for "A"
"\u{41}";

// Unicode escape for "␀"
"\u{2400}";

// Unicode escape for "🦇"
"\u{1f987}"
```

## Building and Running

```bash
# Build the example
make build

# Run the example
./out/run

# Run tests
make test
```

## Expected output

The example demonstrates various kinds of escape sequences.

On a VT100-compatible terminal, this program will display the following line in a bright green color:
```console
Hello, ASCII!
```

Then, on a new line, it will display
```console
Hello, unicode! ␀
```
