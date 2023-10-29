# The Zirco Programming Language

This is the main source code repository for `zrc`, the compiler of the Zirco programming language.

Zirco is a modern programming language coming from a place of C-like semantics and a Rust-like
syntax and type system.

This repository contains the entire Zirco compiler and all of its development work. Language-related
proposals live in this repo as well, because of how rapid development occurs there is no space for
separate proposals.

## Installation

`zrc` is written in Rust, so to obtain a copy of it you will need the stable Rust toolchain which can
be installed from [rustup.rs](https://rustup.rs/).

Once you have done this, you must also have the LLVM dylib available. We do not statically link LLVM currently
because our main developer uses Arch Linux which does not ship the static linking object.

If you want to install `zrc` to your `$PATH`, run `cargo install --path compiler/zrc`. After this point, the `zrc`
executable will be available to use.

Otherwise, you can use `cargo run -- ARGS` to invoke zrc. For more information, run `zrc --help`. You can
compile a Zirco file to LLVM IR with `zrc file.zr`.

Compile a Zirco file: `zrc file.zr`

## Stability

So that Zirco can continue to evolve at a rapid pace, there are **NO STABILITY GUARENTEES** on the current version of Zirco and `zrc`.

All internal compiler APIs are VERY unstable and if you integrate them with your own tooling builds may fail
if you update `zrc`. Zirco code may change semantics or fail to build on a different zrc version. As of now,
all internal libraries and `zrc` is given version `0.1.0` until we begin maintaining stability.

---

![Build Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/build.yml?style=flat-square) ![Coverage](https://img.shields.io/codecov/c/github/zirco-lang/zrc?style=flat-square) ![Test Status](https://img.shields.io/github/actions/workflow/status/zirco-lang/zrc/test.yml?label=tests&style=flat-square) ![Repo Size](https://img.shields.io/github/repo-size/zirco-lang/zrc?style=flat-square) ![open issues](https://img.shields.io/github/issues-raw/zirco-lang/zrc?style=flat-square) ![open PRs](https://img.shields.io/github/issues-pr-raw/zirco-lang/zrc?style=flat-square) ![license](https://img.shields.io/github/license/zirco-lang/zrc?style=flat-square)

![coverage report](https://codecov.io/gh/zirco-lang/zrc/graphs/icicle.svg?token=TI3EP0UNKH)

