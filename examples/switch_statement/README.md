# Switch Statement Example

This example demonstrates the `switch` statement for multi-way branching in Zirco.

## Features Demonstrated

1. **Basic switch**: Matching against integer values
2. **Default case**: Handling unmatched values
3. **Expression matching**: Using expressions in case values
4. **Block statements**: Multiple statements in a case using blocks

## Syntax

```zirco
switch (expression) {
    value1 as Type => statement;
    value2 as Type => statement;
    default => statement;
}
```

**Important Notes**:
- Each case uses the `=>` (fat arrow) syntax
- Case values must be explicitly cast to match the scrutinee's type using `as`
- No fall-through between cases (unlike C)
- Use blocks `{ }` for multiple statements in a case

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
- Day name lookup using switch
- Number classification with default case
- Grade evaluation using expressions in the scrutinee
