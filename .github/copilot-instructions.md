# Copilot Coding Agent Instructions for Zirco Compiler (zrc)

## Repository Overview

**Zirco** is an experimental compiled programming language with a strong type system and modern syntax. The `zrc` repository contains the complete compiler implementation written in Rust. This is a Cargo workspace with 6 crates comprising approximately 15,000 lines of Rust code. The compiler uses LLVM for code generation and follows a traditional compiler pipeline: lexer → parser → type checker → code generator.

**Key Technologies:**

-   Language: Rust (stable toolchain required, nightly for linting/formatting)
-   Build System: Cargo workspace
-   Backend: LLVM 16 (via inkwell)
-   Testing: cargo test + insta snapshot testing (in zrc_codegen)
-   CI/CD: GitHub Actions (build, test, clippy, fmt)

## Critical Build Requirements

### Prerequisites

**ALWAYS install these before building:**

1. **LLVM 16 with Polly** - Required for compilation to succeed

    ```bash
    sudo apt-get update
    sudo apt-get install -y llvm-16 llvm-16-dev libpolly-16-dev
    ```

2. **Rust Stable Toolchain** - For building and testing (required for CI)

    ```bash
    rustup toolchain install stable
    ```

3. **Rust Nightly Toolchain** - For clippy and fmt (required for CI)
    ```bash
    rustup toolchain install nightly --component clippy --component rustfmt
    ```

### Build Commands

**Standard build workflow (ALWAYS run in this exact order) — all commands are REQUIRED before committing:**

```bash
# 1. Build the project (takes ~40-45 seconds on first clean build, ~1-2 seconds incremental)
cargo build

# 2. Run tests (takes ~15-20 seconds)
cargo test

# 3. Test examples
make -C examples test

# 4. Run nightly clippy (takes ~45-50 seconds)
cargo +nightly clippy --all-targets -- -D warnings

# 5. Run nightly fmt (takes <1 second)
cargo +nightly fmt --check
```

**Important Notes:**

-   The build may automatically find LLVM 16 if installed system-wide
-   If build fails with "could not find native static library `Polly`", install `libpolly-16-dev`
-   Debug builds go to `target/debug/zrc`

### Running the Compiler

```bash
# Run with cargo (for development)
cargo run -- <file.zr>
cargo run -- --help

# Run after build
cargo build
target/debug/zrc --help
```

## Project Structure

### Crate Organization (Workspace Members)

The repository follows a standard Cargo workspace layout. All compiler crates are in `compiler/`:

1. **`compiler/zrc/`** - Main compiler binary and CLI

    - Entry point: `src/main.rs`
    - Orchestrates the compilation pipeline
    - CLI argument parsing (clap)
    - Build info generation (shadow-rs)

2. **`compiler/zrc_parser/`** - Lexer and Parser

    - Lexer: tokenization using logos
    - Parser: LALRPOP-based parser
    - AST definitions in `src/ast/`
    - Build script: `build.rs` (generates parser from `.lalrpop`)

3. **`compiler/zrc_typeck/`** - Type Checker

    - Type checking and semantic analysis
    - TAST (Typed AST) definitions in `src/tast/`
    - Type checking logic in `src/typeck/`

4. **`compiler/zrc_codegen/`** - LLVM Code Generator

    - Uses inkwell (LLVM bindings)
    - Generates LLVM IR and object files
    - **Uses insta snapshot testing** (see `src/stmt.rs`, `src/expr.rs` tests)

5. **`compiler/zrc_diagnostics/`** - Error Reporting

    - Diagnostic message formatting
    - Error display utilities

6. **`compiler/zrc_utils/`** - Shared Utilities
    - Line lookup utilities
    - Span tracking
    - Common data structures

### Root Directory Files

-   `Cargo.toml` - Workspace manifest with all member crates
-   `Cargo.lock` - Locked dependencies (committed to repo)
-   `rust-toolchain.toml` - Specifies stable toolchain
-   `rustfmt.toml` - Formatting configuration (uses unstable features)
-   `README.md` - Project documentation
-   `CONTRIBUTING.md` - Contribution guidelines
-   `LICENSE` - GNU GPL v3.0
-   `.github/workflows/` - CI pipeline definitions

### Configuration Files

**Each crate has:**

-   `Cargo.toml` - Dependencies and metadata
-   `clippy.toml` - Clippy linting configuration
    ```toml
    absolute-paths-max-segments = 3
    allowed-idents-below-min-chars = [ "..", "f" ]
    ```

**Root-level configs:**

-   `rustfmt.toml` - Requires nightly due to unstable features:
    ```toml
    unstable_features = true
    imports_granularity = "Crate"
    group_imports = "StdExternalCrate"
    error_on_unformatted = true
    error_on_line_overflow = true
    wrap_comments = true
    ```

## CI/CD Pipeline

The project uses GitHub Actions with 4 workflows in `.github/workflows/`:

### 1. `build.yml` - Build Verification

-   Runs: on push to main, PRs, merge groups
-   Steps: checkout → setup stable Rust → cache → `cargo build`
-   Expected time: ~1-2 minutes

### 2. `test.yml` - Testing with Coverage

-   Runs: on push to main, PRs, merge groups
-   Steps: checkout → setup stable Rust → install cargo-tarpaulin → cache → `cargo tarpaulin -o lcov --engine llvm --skip-clean` → upload to codecov
-   Expected time: ~2-3 minutes
-   Uploads coverage to Codecov

### 3. `clippy.yml` - Linting

-   **Uses nightly Rust** (required for advanced lints)
-   Runs: on push to main, PRs, merge groups
-   Steps: checkout → setup nightly Rust → cache → `cargo clippy --all-targets -- -D warnings`
-   Expected time: ~1-2 minutes
-   **FAILS on any warnings** (note `-D warnings` flag)

### 4. `fmt.yml` - Formatting Check

-   **Uses nightly Rust** (required due to unstable rustfmt features)
-   Runs: on push to main, PRs, merge groups
-   Steps: checkout → setup nightly Rust → `cargo fmt -- --check`
-   Expected time: <30 seconds
-   **FAILS if any files need formatting**

**Critical:** ALWAYS run `cargo +nightly clippy --all-targets -- -D warnings`, `cargo +nightly fmt --check`, and `make -C examples test` before pushing commits, as CI will fail otherwise. All examples MUST pass successfully before committing changes.

## Code Style and Linting

### Clippy Configuration

The project uses **extremely strict** clippy lints configured in `compiler/zrc/src/main.rs`:

-   Enables: `clippy::cargo`, `clippy::nursery`, `clippy::pedantic`, `clippy::missing_docs_in_private_items`, `missing_docs`, plus many additional lints
-   Forbids: `clippy::unwrap_used`, `clippy::todo`, `clippy::unimplemented`, and many others
-   Allows: `clippy::multiple_crate_versions`, `clippy::cargo_common_metadata`, `clippy::module_name_repetitions`

**Key implications:**

-   All items (including private ones) must have documentation comments
-   Cannot use `.unwrap()`, must handle errors properly
-   Cannot have `todo!()` or `unimplemented!()` in committed code
-   Very strict about code quality and best practices

### Documentation Standards

-   All public and private items require doc comments (`///` or `//!`)
-   Functions should document parameters, return values, panics, and safety
-   Use `#[doc(hidden)]` for implementation details that shouldn't appear in docs

## Testing

### Running Tests

```bash
# Run all tests
cargo test

# Run tests for specific crate
cargo test -p zrc_parser
cargo test -p zrc_codegen

# Run with output
cargo test -- --nocapture
```

### Snapshot Testing (zrc_codegen)

-   The code generator uses `insta` for snapshot testing
-   Snapshots stored in `compiler/zrc_codegen/src/snapshots/`
-   Update snapshots: `cargo insta review` (requires `cargo install cargo-insta`)
-   Tests in: `compiler/zrc_codegen/src/stmt.rs`, `compiler/zrc_codegen/src/expr.rs`

### Test Organization

-   Unit tests: inline in source files (`#[cfg(test)] mod tests { ... }`)
-   Integration tests: none currently
-   Doc tests: in doc comments, tested with `cargo test --doc`

### Example Testing Framework

All examples in the `examples/` directory follow a standardized test framework:

**Directory Structure:**
```
examples/
  example_name/
    main.zr                 # The example source code
    Makefile                # Standard Makefile (see below)
    test/
      stdout.txt            # Expected stdout output
      stderr.txt            # Expected stderr output (optional)
      exitcode.txt          # Expected exit code (optional, defaults to 0)
      args.txt              # Command-line arguments (optional)
      stdin.txt             # Standard input (optional)
```

**Standard Makefile Test Target:**

All examples MUST use this exact test implementation:

```makefile
.PHONY: test
test: build
	set +e; \
	if [ -f test/args.txt ]; then args=$$(xargs < test/args.txt); else args=""; fi; \
	if [ -f test/stdin.txt ]; then stdin_file=test/stdin.txt; else stdin_file=/dev/null; fi; \
	./$(OUTDIR)/run $$args < $$stdin_file > test/stdout.actual 2> test/stderr.actual; \
	if [ -f test/exitcode.txt ]; then expected_exitcode=$$(cat test/exitcode.txt); else expected_exitcode=0; fi; \
	exitcode=$$?; \
	status=0; \
	if [ $$exitcode -ne $$expected_exitcode ]; then \
		echo "Expected exit code $$expected_exitcode but got $$exitcode"; \
		status=1; \
	fi; \
	if [ -f test/stdout.txt ]; then \
		diff -u test/stdout.txt test/stdout.actual || { echo "stdout mismatch"; status=1; }; \
	fi; \
	if [ -f test/stderr.txt ]; then \
		diff -u test/stderr.txt test/stderr.actual || { echo "stderr mismatch"; status=1; }; \
	fi; \
	set -e; \
	rm test/stdout.actual test/stderr.actual; \
	exit $$status
```

**How it works:**
1. Runs the compiled example with optional arguments from `test/args.txt`
2. Provides optional stdin from `test/stdin.txt` (defaults to /dev/null)
3. Captures stdout to `test/stdout.actual` and stderr to `test/stderr.actual`
4. Compares actual output with expected files using `diff -u`
5. Checks exit code matches `test/exitcode.txt` (defaults to 0)
6. Cleans up temporary files and reports status

**Do NOT create custom test implementations** - always use the standard framework above. Reference examples: `hello_world`, `fibonacci`, `struct_example`.

## Common Issues and Workarounds

### LLVM-related Build Failures

**Problem:** `error: could not find native static library 'Polly'`
**Solution:** Install `libpolly-16-dev`:

```bash
sudo apt-get install -y libpolly-16-dev
```

**Problem:** LLVM not found
**Solution:** Install LLVM 16 and set environment variable if needed:

```bash
sudo apt-get install -y llvm-16 llvm-16-dev
# If still failing, try:
export LLVM_SYS_160_PREFIX=/usr/lib/llvm-16
cargo build
```

### Clippy/Fmt Failures

**Problem:** CI fails on clippy or fmt
**Solution:** Use nightly toolchain (these commands MUST pass before committing):

```bash
cargo +nightly clippy --all-targets -- -D warnings
cargo +nightly fmt
```

### Build Script Issues

**Problem:** Parser build fails in zrc_parser
**Solution:** The `build.rs` generates parser from `.lalrpop` file. Clean and rebuild:

```bash
cargo clean -p zrc_parser
cargo build -p zrc_parser
```

## Making Changes

### Workflow for Code Changes

1. **Before making changes:**

    ```bash
    # Ensure clean build
    cargo build
    cargo test
    ```

2. **Make your changes**

    - Follow existing code style and documentation patterns
    - Add/update doc comments for all modified items
    - Update tests if changing behavior

3. **Validate changes:**

    ```bash
    # Build and test
    cargo build
    cargo test

    # Test all examples (REQUIRED)
    make -C examples test

    # Check formatting and lints (REQUIRED)
    cargo +nightly fmt
    cargo +nightly clippy --all-targets -- -D warnings

    # If snapshot tests changed, review them
    cargo insta review  # if cargo-insta is installed
    ```

4. **Commit**
    - Use clear, descriptive commit messages **that follow the Semantic Commit Message format**:
        - `feat: add new feature`
        - `fix: resolve bug`
        - `docs: update documentation`
        - `chore: update dependencies`
        - `refactor: improve code structure`
    - Reference issue numbers if applicable

### Adding Dependencies

-   Add to appropriate crate's `Cargo.toml`
-   Prefer well-maintained crates
-   Update documentation if dependency changes project behavior
-   Run `cargo build` and `cargo test` to verify

### Modifying the Compiler Pipeline

-   **Parser changes:** Edit `.lalrpop` file, rebuild triggers parser regeneration
-   **AST changes:** Update `compiler/zrc_parser/src/ast/`
-   **Type system changes:** Update `compiler/zrc_typeck/src/tast/` and `src/typeck/`
-   **Codegen changes:** Update `compiler/zrc_codegen/`, consider snapshot test updates
-   **CLI changes:** Modify `compiler/zrc/src/cli.rs`

## Quick Reference

### Essential Commands (in order of typical use)

```bash
# 1. Build
cargo build                                          # ~40-45s clean build, ~1-2s incremental

# 2. Test
cargo test                                           # ~15-20s

# 3. Test examples (REQUIRED before commit)
make -C examples test                                # varies by example count

# 4. Lint (REQUIRED before commit)
cargo +nightly clippy --all-targets -- -D warnings   # ~45-50s

# 5. Format (REQUIRED before commit)
cargo +nightly fmt                                   # <1s
```

### File Locations Quick Reference

-   Main entry point: `compiler/zrc/src/main.rs`
-   CLI args: `compiler/zrc/src/cli.rs`
-   Parser grammar: `compiler/zrc_parser/src/parser.lalrpop`
-   Lexer: `compiler/zrc_parser/src/lexer.rs`
-   AST: `compiler/zrc_parser/src/ast/`
-   Type checker: `compiler/zrc_typeck/src/typeck/`
-   TAST: `compiler/zrc_typeck/src/tast/`
-   Codegen: `compiler/zrc_codegen/src/`
-   Workflows: `.github/workflows/*.yml`

### Output Formats

The compiler supports multiple output formats via `--emit`:

-   `llvm` (default) - LLVM IR
-   `object` - Object file (`.o`)
-   `asm` - Assembly
-   `ast`, `ast-debug`, `ast-debug-pretty` - AST representation
-   `tast-debug`, `tast-debug-pretty` - Typed AST representation

## Trust These Instructions

These instructions have been validated by:

-   Successfully building the project from a clean state
-   Running all tests and confirming they pass
-   Verifying all CI workflows (build, test, clippy, fmt)
-   Testing with both stable and nightly Rust toolchains
-   Confirming LLVM dependencies and workarounds

**Only search for additional information if:**

-   These instructions are incomplete for your specific task
-   You encounter an error not covered by the troubleshooting section
-   The project structure has changed significantly (check git history)

When in doubt, trust the build commands and CI workflow definitions over other sources.
