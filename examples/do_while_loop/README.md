# Do-While Loop Example

This example demonstrates the `do-while` loop construct in Zirco.

## Features Demonstrated

1. **Basic do-while loop**: Counting from 1 to 5
2. **At-least-once execution**: The loop body executes at least once, even if the condition is initially false
3. **Countdown pattern**: Common use case for do-while loops

## Syntax

```zirco
do {
    // loop body
} while (condition);
```

The key difference from a `while` loop is that the condition is checked *after* each iteration, guaranteeing at least one execution of the loop body.

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

The example demonstrates three different uses of do-while loops:
- Counting from 1 to 5
- A loop that executes once despite a false condition
- A countdown from 5 to 0
