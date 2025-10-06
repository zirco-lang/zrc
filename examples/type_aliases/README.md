# Type Aliases Example

This example demonstrates type aliases in Zirco.

## Features Demonstrated

1. **Type alias declaration**: Creating named aliases for types
2. **Pointer type aliases**: Aliasing pointer types (like string types)
3. **Struct type aliases**: Creating alternative names for struct types

## Syntax

```zirco
// Type alias for a pointer type
type String = *u8;

// Type alias for a struct type
struct Point {
    x: i32,
    y: i32
}
type Position = Point;

// Use the aliases
let name: String = "Alice";
let pos: Position;
```

Type aliases provide:
- More readable code with semantic names
- Easier refactoring (change the underlying type in one place)
- Abstraction over implementation details

## Building and Running

```bash
# Build the example
make build

# Run the example
./out/run

# Run tests
make test
```

## Expected Output

The example demonstrates:
- Using String type alias for `*u8`
- Using Position type alias for Point struct
- Struct construction with type aliases
