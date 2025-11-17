![Build Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/build.yml?style=flat-square) ![Coverage](https://img.shields.io/codecov/c/github/zirco-lang/zrc?style=flat-square) ![Test Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/test.yml?label=tests&style=flat-square) ![Repo Size](https://img.shields.io/github/repo-size/zirco-lang/zrc?style=flat-square) ![open issues](https://img.shields.io/github/issues-raw/zirco-lang/zrc?style=flat-square) ![open PRs](https://img.shields.io/github/issues-pr-raw/zirco-lang/zrc?style=flat-square) ![license](https://img.shields.io/github/license/zirco-lang/zrc?style=flat-square) [![All Contributors](https://img.shields.io/github/all-contributors/zirco-lang/zrc?color=ee8449&style=flat-square)](#contributors)

<div align="center">

![Zirco banner](https://github.com/zirco-lang/assets/blob/main/png/wide-light.png)

A modern programming language focusing on a strong type system and modern syntax

![Alt](https://repobeats.axiom.co/api/embed/ba11a893172e06e66607b7949d986810ab7b42df.svg "Repobeats analytics image")

</div>

## About Zirco

Zirco is an unstable work-in-progress compiled programming language with the goal of having an extremely strong runtime type system with a focus on the ability to easily represent states of a program.

It started out as (and still is) a hobby project for me to learn more about the world of compiler development, so there are no real promises for Zirco's real production usability. However, it also serves as a well-implemented reference for designing a compiler.

As of now, Zirco uses a C-like set of semantics with Rusty syntax.

This repository contains the entire Zirco compiler and all of its development work, including language-related proposals.

## Getting Started

**New to Zirco?** Check out our comprehensive [Getting Started Guide](./docs/GETTING_STARTED.md) for step-by-step instructions on:

-   Installing prerequisites
-   Building the compiler
-   Writing and compiling your first program
-   Using compiler options and output formats
-   Troubleshooting common issues

### Quick Start

For experienced developers who want to get running quickly:

1. **Install prerequisites:**

    ```bash
    # Rust toolchain (if not already installed)
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

    # LLVM 20 with Polly (Ubuntu/Debian)
    sudo apt-get update
    sudo apt-get install -y llvm-20 llvm-20-dev libpolly-20-dev clang-20
    ```

2. **Clone and build:**

    ```bash
    git clone https://github.com/zirco-lang/zrc
    cd zrc
    cargo build
    ```

3. **Install system-wide (optional):**

    ```bash
    cargo install --path compiler/zrc
    ```

4. **Compile your first program:**
    ```bash
    # Create hello.zr with your favorite editor, then:
    zrc --emit object -o hello.o hello.zr
    clang -o hello hello.o -lc
    ./hello
    ```

For detailed instructions and troubleshooting, see the [Getting Started Guide](./docs/GETTING_STARTED.md).

## Language Specification

For a comprehensive guide to the Zirco language syntax, semantics, and behavior, see the [Language Specification](./docs/SPEC.md).

## Usage

If you are directly invoking the compiler with `cargo`, replace `zrc` with `cargo run --` in the below commands.

You can compile a single Zirco file to a `.o` object with `zrc --emit object -o main.o main.zr`. Otherwise, `zrc main.zr` will emit LLVM IR. This is soon to change.

For more usage help, refer to `zrc --help`.

## A Note on Stability

So that Zirco can continue to evolve at a rapid pace, there are **NO STABILITY GUARENTEES** on the current version of Zirco and `zrc`.

All internal compiler APIs are VERY unstable and if you integrate them with your own tooling builds may fail
if you update `zrc`. Zirco code may change semantics or fail to build on a different zrc version. As of now,
all internal libraries and `zrc` is given version `0.1.0` until we begin maintaining stability.

## Contributing

Contributions to Zirco are welcome! Please read [CONTRIBUTING.md](./CONTRIBUTING.md) for more information on how to contribute to the project.

## Frequently Asked Questions (FAQ)

### General Questions

**Q: What is Zirco?**  
A: Zirco is an experimental compiled programming language with a strong type system and modern syntax. It uses LLVM for code generation and is implemented in Rust.

**Q: Is Zirco production-ready?**  
A: No. Zirco is currently unstable and under active development. There are **NO STABILITY GUARANTEES** on the current version. Internal APIs may change, and Zirco code may fail to build on different `zrc` versions.

**Q: What platforms does Zirco support?**  
A: Zirco is primarily developed and tested on Linux and WSL, but it should work on Windows as well. The compiler can target any platform supported by LLVM through the `--target` flag.

### Installation & Building

**Q: What are the prerequisites for building Zirco?**  
A: You need:

-   Git (to clone the repository)
-   An up-to-date stable Rust toolchain (`rustc` and `cargo` via [rustup](https://rustup.rs))
-   LLVM 20 static or dynamic library (including Polly)
-   For linting/formatting: Rust nightly toolchain with clippy and rustfmt components

**Q: I'm getting a "could not find native static library `Polly`" error. How do I fix it?**  
A: Install the LLVM 20 development libraries with Polly support:

```bash
sudo apt-get install -y llvm-20 llvm-20-dev libpolly-20-dev clang-20
```

**Q: How do I build the compiler?**  
A: Clone the repository and run:

```bash
git clone https://github.com/zirco-lang/zrc
cd zrc
cargo build
```

For a release build, use `cargo build --release`.

**Q: How do I install `zrc` to my system?**  
A: Run `cargo install --path compiler/zrc` to install `zrc` to your Cargo `$PATH`.

**Q: LLVM is not being found. What should I do?**  
A: If LLVM 20 is installed but not found, try setting the environment variable:

```bash
export LLVM_SYS_201_PREFIX=/usr/lib/llvm-20
cargo build
```

### Usage

**Q: How do I compile a Zirco program?**  
A: Use `zrc` (or `cargo run --` if not installed):

```bash
zrc main.zr                    # Emits LLVM IR to stdout
zrc --emit object -o main.o main.zr  # Compiles to object file
```

**Q: What output formats does the compiler support?**  
A: The compiler supports multiple output formats via the `--emit` flag:

-   `llvm` (default) - LLVM IR
-   `object` - Object file (.o)
-   `asm` - Assembly
-   `ast`, `ast-debug`, `ast-debug-pretty` - AST representations
-   `tast-debug`, `tast-debug-pretty` - Typed AST representations

**Q: How can I see available compiler options?**  
A: Run `zrc --help` for a complete list of options, including optimization levels, target selection, and debugging flags.

**Q: Where can I find example programs?**  
A: Check the `examples/` directory, which contains working examples like `hello_world`, `fibonacci`, `struct_construction`, and more. You can run all examples with `make -C examples test`.

### Development & Contributing

**Q: How do I run the test suite?**  
A: Run `cargo test` to execute all tests.

**Q: How do I lint and format my code?**  
A: The project requires nightly Rust for linting and formatting:

```bash
cargo +nightly clippy --all-targets -- -D warnings
cargo +nightly fmt
```

Both commands must pass before committing.

**Q: CI is failing on clippy or fmt. What should I do?**  
A: Make sure you're using the nightly toolchain for these commands. Install it with `rustup toolchain install nightly --component clippy --component rustfmt`, then run the commands above.

**Q: What coding standards does the project follow?**  
A: The project uses extremely strict clippy lints, including requirements for documentation on all items (even private ones). All code must be properly documented, and `unwrap()`, `todo!()`, and `unimplemented!()` are forbidden. Check `compiler/zrc/src/main.rs` for the complete lint configuration.

**Q: How does the compiler pipeline work?**  
A: The compiler follows a traditional pipeline:

1. **Lexer** (`zrc_parser`) - Tokenization
2. **Parser** (`zrc_parser`) - AST generation using LALRPOP
3. **Type Checker** (`zrc_typeck`) - Semantic analysis producing a Typed AST (TAST)
4. **Code Generator** (`zrc_codegen`) - LLVM IR generation using inkwell

For a detailed explanation of each stage, see [docs/COMPILER_PIPELINE.md](./docs/COMPILER_PIPELINE.md).

**Q: Where can I find the language specification?**  
A: See [docs/SPEC.md](./docs/SPEC.md) for a comprehensive guide to Zirco's syntax, semantics, and behavior.

### Troubleshooting

**Q: Build times are slow. Is this normal?**  
A: Yes. The first clean build takes ~40-45 seconds due to LLVM dependencies. Incremental builds are much faster (~1-2 seconds).

**Q: Tests are failing that seem unrelated to my changes. What should I do?**  
A: The project may have pre-existing issues. Focus on ensuring your changes don't introduce new failures. If you're unsure, ask in the issue or PR discussion.

**Q: I found a bug or have a feature request. Where should I report it?**  
A: Please [create an issue](https://github.com/zirco-lang/zrc/issues/new/choose) on GitHub. For suggestions, it's recommended to discuss them in an issue before starting work.

**Q: How do I get help with the project?**  
A: You can:

-   Open a GitHub issue for bugs or feature requests
-   Start a GitHub discussion for general questions
-   Check the [CONTRIBUTING.md](./CONTRIBUTING.md) guide
-   For sensitive matters, email [logan@zirco.dev](mailto:logan@zirco.dev]

## Licence & Contact

Zirco is released under the [GNU GPL v3.0](./LICENSE). To contact the maintainer, please use issues or a GitHub discussion. For sensitive matters, please email [logan@zirco.dev](mailto:logan@zirco.dev).

## Code Coverage

If you're curious, here's our test coverage represented as a cool little chart:

![coverage report](https://codecov.io/gh/zirco-lang/zrc/graphs/icicle.svg?token=TI3EP0UNKH)

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/thetayloredman"><img src="https://avatars.githubusercontent.com/u/26350849?v=4?s=100" width="100px;" alt="Logan Devine"/><br /><sub><b>Logan Devine</b></sub></a><br /><a href="#code-thetayloredman" title="Code">💻</a> <a href="#design-thetayloredman" title="Design">🎨</a> <a href="#ideas-thetayloredman" title="Ideas, Planning, & Feedback">🤔</a> <a href="#maintenance-thetayloredman" title="Maintenance">🚧</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ValShaped"><img src="https://avatars.githubusercontent.com/u/8754234?v=4?s=100" width="100px;" alt="Val"/><br /><sub><b>Val</b></sub></a><br /><a href="#ideas-ValShaped" title="Ideas, Planning, & Feedback">🤔</a> <a href="#code-ValShaped" title="Code">💻</a> <a href="#bug-ValShaped" title="Bug reports">🐛</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
