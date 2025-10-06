# Union Types Example

This example demonstrates union types in Zirco.

## Features Demonstrated

1. **Union declaration**: Defining a union type with multiple fields
2. **Union usage**: Accessing union fields
3. **Memory layout**: Understanding union size (equals the largest field)

## Syntax

```zirco
// Define a union type
union Value {
    i: i32,
    b: u8
}

// Use the union
let val: Value;
val.i = 42;  // Set one field
```

**Important Notes**:
- Only one field is active at a time
- Reading an inactive field is undefined behavior
- The union's size is the size of its largest field
- Unions provide memory-efficient storage for values that can be one of several types

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
- Using a union with i32 field
- Using a union with u8 field
- Comparing the size of the union to its constituent types
