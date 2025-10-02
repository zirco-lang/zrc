# Struct Construction Parser Example

This example demonstrates that the new struct construction syntax successfully parses in Zirco.

## Features Demonstrated

The parser correctly handles:

1. **Named Struct Construction**: `new Point { x: 0, y: 0 }`
2. **Multi-field Structs**: `new Color { r: 255, g: 0, b: 0 }`
3. **Anonymous Struct Types**: `new struct { x: i32, y: i32 } { x: 5, y: 15 }`
4. **Empty Structs**: `new EmptyStruct { }`
5. **Expression Values**: `new Point { x: 1 + 2, y: 3 * 4 }`

## Viewing the AST

To see the parsed AST output:

```bash
../../target/debug/zrc main.zr --emit ast
```

This will show that the parser successfully recognizes and processes the `new` keyword syntax for struct construction.

## Example Code

The `main.zr` file contains multiple examples of struct construction syntax:

```zirco
struct Point {
    x: i32,
    y: i32
}

fn main() {
    // Named struct construction
    let origin = new Point { x: 0, y: 0 };
    
    // Multi-field struct
    let red = new Color { r: 255, g: 0, b: 0 };
    
    // Anonymous struct
    let p = new struct { x: i32, y: i32 } { x: 5, y: 15 };
}
```

## Implementation Status

**✅ Parser**: Fully implemented and tested  
**⚠️ Type Checker**: Placeholder implementation (returns error message)  
**⚠️ Code Generator**: Not yet implemented  

## Testing

The parser functionality is tested with unit tests in `compiler/zrc_parser/src/parser.rs`:

```bash
cd ../../
cargo test -p zrc_parser
```

All struct construction parser tests pass successfully.

## Viewing Parsed Output

You can verify the parser works correctly by viewing the AST:

```bash
cd /home/runner/work/zrc/zrc
cargo run --bin zrc -- examples/struct_construction/main.zr --emit ast
```

**Expected output includes**:
```
let origin = (new Point { x: (0), y: (0) });
let red = (new Color { r: (255), g: (0), b: (0) });
```

## Next Steps

To make this example fully executable, the following components need to be implemented:

1. Type checker support for struct construction expressions
2. TAST (Typed AST) variant for struct construction
3. Code generation for struct construction in LLVM IR

The parser foundation is complete and ready for these next implementation steps.

