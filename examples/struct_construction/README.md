# Struct Construction Example

This example demonstrates the struct construction syntax in Zirco using the `new` keyword.

## Features Demonstrated

The example shows fully working struct construction with:

1. **Named Struct Construction**: `new Point { x: 0, y: 0 }`
2. **Multi-field Structs**: `new Color { r: 255, g: 0, b: 0 }`
3. **Using Constructed Structs**: Accessing and manipulating fields of constructed struct instances
4. **Expression Values**: Using struct fields in calculations

## Syntax

```zirco
// Define a struct type
struct Point {
    x: i32,
    y: i32
}

// Construct an instance using the new keyword
let origin = Point { x: 0, y: 0 };

// Access fields
let x_value = origin.x;
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

The example demonstrates various struct construction patterns and prints:

```
=== Struct Construction Examples ===

1. Named struct construction:
Point { x: 0, y: 0 }
Point { x: 50, y: 50 }

2. Multi-field struct construction:
Color { r: 255, g: 0, b: 0 }
Color { r: 0, g: 255, b: 0 }
Color { r: 0, g: 0, b: 255 }

3. Using struct fields in calculations:
Distance squared between Point { x: 3, y: 4 }
and Point { x: 6, y: 8 }
is 25

=== All tests passed! ===
```

## Implementation Status

**✅ Parser**: Fully implemented and tested  
**✅ Type Checker**: Fully implemented  
**✅ Code Generator**: Fully implemented

All components are complete and working!

## Implementation Notes

This example showcases the `new` keyword syntax which was chosen to avoid LALR(1) parser ambiguities. The syntax is:

-   `new TypeName { field1: value1, field2: value2, ... }`

This works for both named types (like `Point` and `Color` in this example) and anonymous struct types.

The type checker ensures:

-   All required struct fields are initialized
-   Field types match the struct definition
-   No duplicate field initializations
-   The constructed type is indeed a struct or union
