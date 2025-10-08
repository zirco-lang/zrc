# Enum Construction Example

This example demonstrates the enum construction syntax in Zirco using the `new` keyword with variant names.

## Features Demonstrated

The example shows fully working enum construction with:

1. **Enum Variant Construction**: `new Result { Ok: 42i32 }`
2. **Multiple Variants**: Creating different variants of the same enum
3. **Enums in Functions**: Using enums as return types and constructing them in functions
4. **Discriminant Access**: Accessing the internal discriminant field to identify variants

## Syntax

```zirco
// Define an enum type
enum Result {
    Ok: i32,
    Error: i32
}

// Construct an instance using variant syntax
let success = new Result { Ok: 42i32 };
let failure = new Result { Error: 404i32 };

// Access discriminant (0 for first variant, 1 for second, etc.)
let which_variant = success.__discriminant__;
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

The example demonstrates enum construction patterns and should complete successfully with exit code 0.

## Implementation Status

**✅ Parser**: Fully implemented and tested  
**✅ Type Checker**: Fully implemented with variant syntax support  
**✅ Code Generator**: Fully implemented  

All components are complete and working!

## Implementation Notes

Enums in Zirco are internally represented as structs with:
- `__discriminant__`: A `usize` field indicating which variant is active (0-indexed)
- `__value__`: A union containing all possible variant values

The `new MyEnum { Variant: value }` syntax is syntactic sugar that:
1. Identifies which variant is being constructed (e.g., "Ok" vs "Error")
2. Calculates the discriminant index
3. Constructs the underlying struct with the discriminant and casted value

This design allows for:
- Type-safe enum construction
- Pattern matching (via `match` statements)
- Efficient memory layout (union uses space of largest variant)
