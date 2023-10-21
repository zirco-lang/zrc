# The Zirco Programming Language

This is the main source code repository for `zrc`, the compiler of the Zirco programming language.

It is implemented in Rust, with the help of the Logos lexer generator and LALRPOP parser generator.

To install the compiler to a binary in your `$PATH` (with the Rust toolchain installed), you can run
`cargo install --path compiler/zrc`. After this point, the `zrc` executable will be available to use.

Otherwise, you can replace `zrc` with `cargo run --` in all commands below.

Compile a Zirco file: `zrc file.zr`

![Build Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/build.yml?style=flat-square) ![Coverage](https://img.shields.io/codecov/c/github/zirco-lang/zrc?style=flat-square) ![Test Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/test.yml?label=tests&style=flat-square) ![Repo Size](https://img.shields.io/github/repo-size/zirco-lang/zrc?style=flat-square) ![open issues](https://img.shields.io/github/issues-raw/zirco-lang/zrc?style=flat-square) ![open PRs](https://img.shields.io/github/issues-pr-raw/zirco-lang/zrc?style=flat-square) ![license](https://img.shields.io/github/license/zirco-lang/zrc?style=flat-square)

![coverage report](https://codecov.io/gh/zirco-lang/zrc/graphs/icicle.svg?token=TI3EP0UNKH)
