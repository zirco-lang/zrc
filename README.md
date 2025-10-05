![Build Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/build.yml?style=flat-square) ![Coverage](https://img.shields.io/codecov/c/github/zirco-lang/zrc?style=flat-square) ![Test Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/test.yml?label=tests&style=flat-square) ![Repo Size](https://img.shields.io/github/repo-size/zirco-lang/zrc?style=flat-square) ![open issues](https://img.shields.io/github/issues-raw/zirco-lang/zrc?style=flat-square) ![open PRs](https://img.shields.io/github/issues-pr-raw/zirco-lang/zrc?style=flat-square) ![license](https://img.shields.io/github/license/zirco-lang/zrc?style=flat-square)

<div align="center">

![Zirco banner](https://github.com/zirco-lang/assets/blob/main/png/wide-light.png)

A modern programming language focusing on a strong type system and modern syntax

[Repository Statistics](https://repo-tracker.com/r/gh/zirco-lang/zrc)

</div>

## About Zirco

Zirco is an unstable work-in-progress compiled programming language with the goal of having an extremely strong runtime type system with a focus on the ability to easily represent states of a program.

It started out as (and still is) a hobby project for me to learn more about the world of compiler development, so there are no real promises for Zirco's real production usability. However, it also serves as a well-implemented reference for designing a compiler.

As of now, Zirco uses a C-like set of semantics with Rusty syntax.

This repository contains the entire Zirco compiler and all of its development work, including language-related proposals.

## Getting Started

### Prerequisites

To function, Zirco has a few prerequisite programs that must be available. We recommend using Zirco on a Linux or WSL system, but it should work on Windows.

- **a copy of `zrc`'s source code**  
  Git is the primary tool used for development of Zirco, so the preferred method of obtaining this is by using [Git](https://git-scm.com/) and cloning our repository. There are alternative methods, but these are not listed as they make updating much more difficult.
- **an up-to-date stable Rust toolchain**  
  Zirco is implemented in Rust, so an up-to-date version of `rustc` and `cargo` is required. These can be obtained with `rustup`, which you can [install here](https://rustup.rs).
- **the LLVM static or dynamic library**  
  Zirco's code generator (`zrc_codegen`) relies on [LLVM](https://llvm.org/) to allow efficient and simple code generation. For this reason, you need the static or dynamic LLVM library. If you link with the static library, the dynamic library will not be needed at runtime. If you do not use the static library, all hosts running your built compiler will need the dynamic library. The installation of these libraries depends on your platform — in the future we will document this better.

### Compilation

If you decided to use Git to download the repository, run `git clone https://github.com/zirco-lang/zrc` in a command line. Then, run `cd zrc` to move into that directory. Otherwise, obtain a terminal within our codebase in your preferred method.

Then, to compile `zrc` and all of its dependencies in debug mode, simply run `cargo build`. You can execute the built compiler with `cargo run -- ARGS`. If you are wanting to produce a release build, run `cargo build --release` and an optimized executable will be found in `target/release/zrc`.

If you would like to install `zrc` to your Cargo `$PATH`, run `cargo install --path compiler/zrc`. This will allow you to use `zrc` just like other executables on your system.

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

## Licence & Contact

Zirco is released under the [GNU GPL v3.0](./LICENSE). To contact the maintainer, please use issues or a GitHub discussion. For sensitive matters, please email [zirco@0xlogn.dev](mailto:zirco@0xlogn.dev) and optionally encrypt your messages [using my PGP key](https://0xlogn.dev/gpg).

## Code Coverage

If you're curious, here's our test coverage represented as a cool little chart:

![coverage report](https://codecov.io/gh/zirco-lang/zrc/graphs/icicle.svg?token=TI3EP0UNKH)

