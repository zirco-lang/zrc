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

The benchmark suite is organized into four categories:

1. **Parsing** - Measures lexing and parsing performance
   - `parse_simple` - Simple hello world program
   - `parse_fibonacci` - Recursive fibonacci function
   - `parse_struct` - Struct definition and usage

2. **Type Checking** - Measures type checking performance
   - `typeck_simple` - Simple hello world program
   - `typeck_fibonacci` - Recursive fibonacci function
   - `typeck_struct` - Struct definition and usage

3. **Code Generation** - Measures LLVM IR generation performance
   - `codegen_simple` - Simple hello world program
   - `codegen_fibonacci` - Recursive fibonacci function
   - `codegen_struct` - Struct definition and usage

4. **End-to-End** - Measures complete compilation pipeline performance
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
