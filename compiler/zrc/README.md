# `zrc` --- the official Zirco compiler

This crate serves as the frontend and binary for `zrc`, the official compiler for the Zirco
programming language.

When running `zrc`, you simply invoke it with one argument, which is the file to compile.

## Performance Benchmarks

This crate includes comprehensive performance benchmarks for measuring the compiler's performance
across different compilation stages. The benchmarks use [Criterion.rs](https://github.com/bheisler/criterion.rs)
to provide detailed statistical analysis.

### Running Benchmarks

To run all benchmarks:

```bash
cargo bench --bench compilation
```

To run specific benchmark groups:

```bash
# Only parsing benchmarks
cargo bench --bench compilation parsing

# Only type checking benchmarks
cargo bench --bench compilation typechecking

# Only code generation benchmarks
cargo bench --bench compilation codegen

# Only end-to-end benchmarks
cargo bench --bench compilation end_to_end
```

To run a specific benchmark:

```bash
cargo bench --bench compilation parse_simple
```

### Benchmark Categories

The benchmark suite includes **22 benchmarks** organized into four categories:

1. **Parsing** (6 benchmarks) - Measures lexing and parsing performance
   - `parse_simple` - Simple hello world program
   - `parse_fibonacci` - Recursive fibonacci function
   - `parse_struct` - Struct definition and usage
   - `parse_pointer` - Pointer operations and swap
   - `parse_globals` - Global variables and constants
   - `parse_loop` - For loop implementation

2. **Type Checking** (6 benchmarks) - Measures type checking performance
   - `typeck_simple` - Simple hello world program
   - `typeck_fibonacci` - Recursive fibonacci function
   - `typeck_struct` - Struct definition and usage
   - `typeck_pointer` - Pointer operations and swap
   - `typeck_globals` - Global variables and constants
   - `typeck_loop` - For loop implementation

3. **Code Generation** (6 benchmarks) - Measures LLVM IR generation performance
   - `codegen_simple` - Simple hello world program
   - `codegen_fibonacci` - Recursive fibonacci function
   - `codegen_struct` - Struct definition and usage
   - `codegen_pointer` - Pointer operations and swap
   - `codegen_globals` - Global variables and constants
   - `codegen_loop` - For loop implementation

4. **End-to-End** (4 benchmarks) - Measures complete compilation pipeline performance
   - `e2e_simple` - Simple hello world program (unoptimized)
   - `e2e_fibonacci` - Recursive fibonacci function (unoptimized)
   - `e2e_struct` - Struct definition and usage (unoptimized)
   - `e2e_fibonacci_optimized` - Fibonacci with aggressive optimization

### Benchmark Results

Benchmark results are saved to `target/criterion/` and include:
- HTML reports with detailed statistics and charts
- Comparison with previous runs to detect performance regressions
- Statistical analysis including mean, median, and standard deviation

Open `target/criterion/report/index.html` in a browser to view detailed results.

### Automated Performance Tracking

A GitHub Actions workflow automatically runs benchmarks on all pull requests to detect performance changes:

- Compares PR performance against the base branch
- Posts results as a comment on the PR
- Archives full benchmark results as artifacts
- Helps identify performance regressions before merging

The workflow can also be triggered manually from the Actions tab.
