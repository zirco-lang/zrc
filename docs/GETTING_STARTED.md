# Getting Started with Zirco

Welcome to Zirco! This guide will help you get up and running with the Zirco programming language and its compiler, `zrc`. By the end of this guide, you'll have a working Zirco development environment and will have compiled your first Zirco program.

## What You'll Learn

-   How to install all necessary prerequisites
-   How to build the Zirco compiler from source
-   How to write and compile your first Zirco program
-   How to use the compiler's various output formats
-   How to troubleshoot common issues

## Prerequisites

Before you begin, you'll need to install several tools and libraries. Zirco works best on Linux or WSL (Windows Subsystem for Linux), but it can also be built on macOS with some additional configuration. Zirco is not currently supported on native Windows.

### 1. Git

Git is used to clone the Zirco repository. Most systems have it pre-installed, but if not:

**Linux (Ubuntu/Debian):**

```bash
sudo apt-get update
sudo apt-get install -y git
```

**macOS:**

```bash
# Using Homebrew
brew install git
```

### 2. Rust Toolchain

Zirco is written in Rust, so you need an up-to-date Rust toolchain (including `rustc` and `cargo`).

**All Platforms:**

Install Rust using rustup:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Or visit [rustup.rs](https://rustup.rs) for alternative installation methods.

After installation, verify your Rust installation:

```bash
rustc --version
cargo --version
```

You should see version information for both commands.

### 3. LLVM dependencies

Zirco's code generator relies on LLVM 20 for compilation. You need the LLVM static or dynamic library with Polly support.

**Linux (Ubuntu/Debian):**

```bash
sudo apt-get update
sudo apt-get install -y llvm-20 llvm-20-dev libpolly-20-dev clang-20 build-essential libssl-dev pkg-config libzstd-dev
```

**macOS:**

```bash
brew install llvm@20
```

After installation on macOS, you may need to set the LLVM prefix:

```bash
export LLVM_SYS_201_PREFIX=$(brew --prefix llvm@20)
```

**Windows:**
LLVM installation on Windows is more complex. We recommend using WSL (Windows Subsystem for Linux) for the best experience.

### 4. Optional: Rust Nightly (for Development)

If you plan to contribute to Zirco or run linting/formatting tools, install the nightly toolchain:

```bash
rustup toolchain install nightly --component clippy --component rustfmt
```

## Installation

### Alternative Method: Using Zircon Toolchain Installer

If you prefer a simpler installation and do not plan on contributing to Zirco, you can install it using the Zircon toolchain installer. For more information, visit the [Zircon GitHub repository](https://github.com/zirco-lang/zircon).

### Step 1: Clone the Repository

```bash
git clone https://github.com/zirco-lang/zrc
cd zrc
```

### Step 2: Build the Compiler

Build the Zirco compiler in debug mode (faster to compile, slower to run):

```bash
cargo build
```

The first build takes about 40-45 seconds due to LLVM dependencies. Subsequent builds are much faster (1-2 seconds).

For a release build (optimized, faster execution):

```bash
cargo build --release
```

### Step 3: Verify the Installation

Run the test suite to ensure everything is working correctly:

```bash
cargo test
```

Test the example programs:

```bash
make -C examples test
```

### Step 4: Install `zrc` to Your System (Optional)

To use `zrc` as a system command:

```bash
cargo install --path compiler/zrc
```

This installs `zrc` to your Cargo bin directory (usually `~/.cargo/bin`), which should be in your `$PATH`.

Verify the installation:

```bash
zrc --help
```

If `zrc --help` works, you're all set! If not installed system-wide, you can run the compiler using:

```bash
cargo run -- --help
```

## Your First Zirco Program

Let's create and compile a simple "Hello, World!" program.

### Step 1: Create a Zirco File

Create a file named `hello.zr`:

```zirco
fn printf(format: *u8, ...) -> i32;

fn main() {
    printf("Hello, World!\n");
}
```

This program:

-   Declares the C `printf` function (external function declaration)
-   Defines a `main` function that calls `printf` to print "Hello, World!"

### Step 2: Compile to Object File

Compile your program to an object file:

```bash
# If you installed zrc system-wide:
zrc --emit object -o hello.o hello.zr

# Or using cargo:
cargo run -- --emit object -o hello.o hello.zr
```

### Step 3: Link and Create Executable

Link the object file with the C library to create an executable:

```bash
clang -o hello hello.o -lc
```

### Step 4: Run Your Program

```bash
./hello
```

You should see:

```
Hello, World!
```

Congratulations! You've just compiled and run your first Zirco program! 🎉

## Understanding Compiler Output Formats

The Zirco compiler supports multiple output formats via the `--emit` flag:

### LLVM IR (Default)

LLVM Intermediate Representation - useful for understanding how your code is compiled:

```bash
zrc hello.zr
# or
zrc --emit llvm hello.zr
```

This outputs LLVM IR to stdout. To save to a file:

```bash
zrc --emit llvm -o hello.ll hello.zr
```

### Object File

Compiled object code that can be linked with a C compiler:

```bash
zrc --emit object -o hello.o hello.zr
```

### Assembly

Native assembly code for your target architecture:

```bash
zrc --emit asm -o hello.s hello.zr
```

### AST (Abstract Syntax Tree)

View the parsed structure of your program:

```bash
zrc --emit ast hello.zr              # Formatted as Zirco code
zrc --emit ast-debug hello.zr        # Rust debug format
zrc --emit ast-debug-pretty hello.zr # Rust debug format with indentation
```

### TAST (Typed Abstract Syntax Tree)

View the type-checked structure with type information:

```bash
zrc --emit tast hello.zr              # Formatted as Zirco code
zrc --emit tast-debug hello.zr        # Rust debug format
zrc --emit tast-debug-pretty hello.zr # Rust debug format with indentation
```

## Compiler Options

### Optimization Levels

Control how much the compiler optimizes your code:

```bash
zrc -O 0 hello.zr  # No optimization (fastest compilation)
zrc -O 1 hello.zr  # Basic optimization
zrc -O 2 hello.zr  # Default optimization (balanced)
zrc -O 3 hello.zr  # Aggressive optimization (best performance)
```

### Target Architecture

Compile for a different architecture:

```bash
zrc --target x86_64-unknown-linux-gnu hello.zr
zrc --target aarch64-unknown-linux-gnu hello.zr
```

### Debug Information

Include debugging information in the output:

```bash
zrc -g --emit object -o hello.o hello.zr
```

### Complete Example with Options

```bash
zrc --emit object -o hello.o -O 3 -g --target x86_64-unknown-linux-gnu hello.zr
```

This command:

-   Emits an object file (`--emit object`)
-   Outputs to `hello.o` (`-o hello.o`)
-   Uses aggressive optimization (`-O 3`)
-   Includes debug information (`-g`)
-   Targets x86_64 Linux (`--target x86_64-unknown-linux-gnu`)

## More Examples

The `examples/` directory contains several working Zirco programs demonstrating different language features:

### Fibonacci Sequence

```bash
cd examples/fibonacci
make build
./out/run
```

### Struct Construction

```bash
cd examples/struct_construction
make build
./out/run
```

### Pointer Operations

```bash
cd examples/pointer_swap
make build
./out/run
```

### Run All Examples

From the repository root:

```bash
make -C examples test
```

## Troubleshooting

### Build Error: "could not find native static library `Polly`"

**Problem:** LLVM Polly library is not installed.

**Solution:** Install the LLVM 16 development libraries:

```bash
# Ubuntu/Debian:
sudo apt-get install -y llvm-16 llvm-16-dev libpolly-16-dev

# macOS:
brew install llvm@16
```

### Build Error: LLVM Not Found

**Problem:** LLVM 16 is installed but the build system can't find it.

**Solution:** Set the `LLVM_SYS_160_PREFIX` environment variable:

```bash
# Linux (typical location):
export LLVM_SYS_160_PREFIX=/usr/lib/llvm-16

# macOS with Homebrew:
export LLVM_SYS_160_PREFIX=$(brew --prefix llvm@16)

# Then build:
cargo build
```

### Slow Build Times

**Problem:** Initial build is very slow.

**Solution:** This is normal! The first clean build takes ~40-45 seconds due to LLVM dependencies. Incremental builds are much faster (~1-2 seconds). Consider using `cargo build --release` less frequently and `cargo build` for development.

### "zrc: command not found" After Installation

**Problem:** `zrc` command is not found after `cargo install`.

**Solution:** Ensure `~/.cargo/bin` is in your `$PATH`:

```bash
# Add to your ~/.bashrc or ~/.zshrc:
export PATH="$HOME/.cargo/bin:$PATH"

# Then reload your shell:
source ~/.bashrc  # or source ~/.zshrc
```

### Object File Linking Fails

**Problem:** `clang` or `gcc` not found when linking object files.

**Solution:** Install a C compiler:

```bash
# Ubuntu/Debian:
sudo apt-get install -y clang

# macOS:
xcode-select --install
```

## Next Steps

Now that you have Zirco up and running, here's what you can explore next:

1. **Language Specification**: Read the comprehensive [Language Specification](./SPEC.md) to learn about Zirco's syntax, type system, and semantics.

2. **Examples**: Explore the `examples/` directory to see more complex Zirco programs:

    - `hello_world/` - Basic program structure
    - `fibonacci/` - Recursion and conditionals
    - `struct_construction/` - Working with structs
    - `loop_example/` - For loops and iteration
    - `pointer_swap/` - Pointer manipulation
    - `global_variables/` - Global variable usage

3. **Contributing**: If you'd like to contribute to Zirco, check out [CONTRIBUTING.md](../.github/CONTRIBUTING.md) for guidelines on:

    - Setting up a development environment
    - Running tests and linters
    - Code style requirements
    - Submitting pull requests

4. **Compiler Internals**: Learn about the compiler pipeline:

    - **Lexer** (`zrc_parser`) - Tokenization
    - **Parser** (`zrc_parser`) - AST generation using LALRPOP
    - **Type Checker** (`zrc_typeck`) - Semantic analysis
    - **Code Generator** (`zrc_codegen`) - LLVM IR generation

5. **Get Help**:
    - [Open an issue](https://github.com/zirco-lang/zrc/issues) for bugs or questions
    - Check the [FAQ](../README.md#frequently-asked-questions-faq) for common questions
    - Email [logan@zirco.dev](mailto:logan@zirco.dev) for sensitive matters

## Important Notes

### Stability Warning

⚠️ **Zirco is experimental and unstable.** There are **NO STABILITY GUARANTEES** on the current version. This means:

-   Internal compiler APIs may change without notice
-   Zirco code may fail to build on different `zrc` versions
-   Language semantics may change
-   This is not recommended for production use

Zirco is a learning project and reference implementation for compiler design. Use it for education, experimentation, and fun!

### Platform Support

While Zirco is primarily developed and tested on Linux and WSL, it should work on:

-   ✅ Linux (Ubuntu, Debian, Fedora, Arch, etc.)
-   ✅ WSL (Windows Subsystem for Linux)
-   ✅ macOS (with some environment variable configuration)
-   ⚠️ Windows (recommended to use WSL for best experience)

The compiler can target any platform supported by LLVM through the `--target` flag.

## Summary

You've now learned how to:

✅ Install all prerequisites for Zirco development  
✅ Build the Zirco compiler from source  
✅ Write and compile a Zirco program  
✅ Use different output formats and compiler options  
✅ Troubleshoot common issues

Happy coding with Zirco! 🚀
