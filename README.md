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

## Toolchain Installation via Zircon

Zirco can be installed via the Zircon toolchain installer. For more information, visit the
[Zircon GitHub repository](https://github.com/zirco-lang/zircon).

If you aim to contribute to the Zirco compiler itself, please follow the instructions below.

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

    # LLVM 20 with Polly + other dependencies (Ubuntu/Debian)
    sudo apt-get update
    sudo apt-get install -y llvm-20 llvm-20-dev libpolly-20-dev clang-20 build-essential libssl-dev pkg-config libzstd-dev
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

Contributions to Zirco are welcome! Please read [CONTRIBUTING.md](./.github/CONTRIBUTING.md) for more information on how to contribute to the project.

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
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ilikestohack"><img src="https://avatars.githubusercontent.com/u/47259933?v=4?s=100" width="100px;" alt="charmines"/><br /><sub><b>charmines</b></sub></a><br /><a href="#code-ilikestohack" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/bobosii"><img src="https://avatars.githubusercontent.com/u/116455540?v=4?s=100" width="100px;" alt="Emirhan DAY"/><br /><sub><b>Emirhan DAY</b></sub></a><br /><a href="#code-bobosii" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://ali-m07.github.io/resume/"><img src="https://avatars.githubusercontent.com/u/60949510?v=4?s=100" width="100px;" alt="Ali Mansouri"/><br /><sub><b>Ali Mansouri</b></sub></a><br /><a href="#code-ali-m07" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://agentdid127.com"><img src="https://avatars.githubusercontent.com/u/27520129?v=4?s=100" width="100px;" alt="Cory Borek"/><br /><sub><b>Cory Borek</b></sub></a><br /><a href="#ideas-CoryBorek" title="Ideas, Planning, & Feedback">🤔</a> <a href="#bug-CoryBorek" title="Bug reports">🐛</a> <a href="#code-CoryBorek" title="Code">💻</a> <a href="#platform-CoryBorek" title="Packaging/porting to new platform">📦</a> <a href="#maintenance-CoryBorek" title="Maintenance">🚧</a> <a href="#plugin-CoryBorek" title="Plugin/utility libraries">🔌</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ddawn-ll"><img src="https://avatars.githubusercontent.com/u/245093789?v=4?s=100" width="100px;" alt="ddawn-ll"/><br /><sub><b>ddawn-ll</b></sub></a><br /><a href="#content-ddawn-ll" title="Content">🖋</a> <a href="#design-ddawn-ll" title="Design">🎨</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ImTheShrub"><img src="https://avatars.githubusercontent.com/u/234684568?v=4?s=100" width="100px;" alt="ImTheShrub"/><br /><sub><b>ImTheShrub</b></sub></a><br /><a href="#ideas-ImTheShrub" title="Ideas, Planning, & Feedback">🤔</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://yam.codes"><img src="https://avatars.githubusercontent.com/u/2014360?v=4?s=100" width="100px;" alt="Yam C Borodetsky"/><br /><sub><b>Yam C Borodetsky</b></sub></a><br /><a href="#doc-yamcodes" title="Documentation">📖</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
