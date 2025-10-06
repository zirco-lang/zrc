# Break and Continue Example

This example demonstrates the `break` and `continue` statements in Zirco loops.

## Features Demonstrated

1. **Break statement**: Exit a loop early when a condition is met
2. **Continue statement**: Skip the rest of the current iteration and move to the next
3. **Combined usage**: Using both break and continue in the same loop
4. **Loop control flow**: Various patterns for controlling loop execution

## Syntax

```zirco
// Break - exits the innermost loop
while (condition) {
    if (exit_condition) {
        break;
    }
}

// Continue - skips to next iteration
for (let i = 0; i < 10; i = i + 1) {
    if (skip_condition) {
        continue;
    }
    // This is skipped when continue executes
}
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
- Finding the first number divisible by 7 using `break`
- Printing only odd numbers using `continue`
- Combining both in a single loop
- Search pattern with early exit
