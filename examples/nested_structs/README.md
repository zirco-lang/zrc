# Nested Structs Example

This example demonstrates nested struct types in Zirco.

## Features Demonstrated

1. **Nested field access**: Accessing fields of structs within structs
2. **Multiple levels of nesting**: Structs containing other struct types
3. **Struct construction with nesting**: Using the `new` keyword with nested structs

## Syntax

```zirco
struct Point {
    x: i32,
    y: i32
}

struct Rectangle {
    top_left: Point,
    bottom_right: Point
}

// Access nested fields
let rect: Rectangle;
rect.top_left.x = 0;
rect.top_left.y = 10;

// Construct with nested structs
let p1 = new Point { x: 5, y: 5 };
let rect2 = new Rectangle { top_left: p1, bottom_right: p2 };
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

## Expected Output

The example demonstrates:
- Rectangle with nested Point structs
- Company with nested Person struct
- Using struct construction syntax with nested types
