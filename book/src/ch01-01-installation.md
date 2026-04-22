# Installation

The first step is to install the Zirco compiler (`zrc`) and its associated tools and libraries. We
call this the "Zirco toolchain".

The toolchain has host support for the following targets:

- `x86_64-unknown-linux-gnu` (Linux on x86_64)
- `aarch64-unknown-linux-gnu` (Linux on ARM64)
- `x86_64-apple-darwin` (macOS on x86_64, deprecated)
- `aarch64-apple-darwin` (macOS on Apple Silicon)

Other platforms may be supported via cross-compilation or by building Zirco with a custom LLVM
build, but this is not currently documented.

Zirco does not support Windows, but the compiler runs normally on Windows via WSL.

> [!NOTE]
> **Command Line Notation**
>
> Throughout the book, we’ll show some commands used in the terminal.
> Lines that you should enter in a terminal all start with `$`. You don’t need to type the `$`
> character; it’s the command line prompt shown to indicate the start of each command. Lines that
> don’t start with `$` typically show the output of the previous command.

## Selecting an Installation Method

The recommended way to install the Zirco toolchain is via the `zircon` toolchain installer, which
allows you to easily install and manage multiple versions of the Zirco toolchain.

Other installation methods are included below, alongside instructions for their use. If you don't
know what installation method to use, we recommend using `zircon`.

| Installation Method                                       | Description                                       |
| --------------------------------------------------------- | ------------------------------------------------- |
| [`zircon`](#install-via-zircon)                           | Recommended, simple CLI tool                      |
| [debian package](#install-via-debian-package)             | For Debian-based Linux distros                    |
| [nix flake](#install-via-nix)                             | For Nix users                                     |
| [download prebuilt binaries](#download-prebuilt-binaries) | For users who don't want to use a package manager |
| [build from source](#build-from-source)                   | For users who want to build Zirco themselves      |

## Install via `zircon`

Zircon requires `libarchive`, curl, and openssl to be installed on your system. On Debian-based
Linux distros, you can install these dependencies with the following command:

```
$ sudo apt install libarchive-dev curl openssl
```

Now, you can run the Zirco bootstrap script to set up `zircon` in to `~/.zircon`.

```
$ curl -sL https://zirco.dev/zstrap.sh | bash
✓ Zircon installed successfully

Next steps:
1. Add the following line to your shell profile (e.g., ~/.bashrc, ~/.zshrc):
   source <($HOME/.zircon/self/bin/zircon env)
2. Restart your terminal or run 'source ~/.bashrc' (or the appropriate command for your shell) to apply the changes.
3. Run 'zircon install' to install the latest Zirco toolchain.
```

Add the required environment variables to your shell profile, as `zircon` will not do it for you.

| Shell | Command                                                                      |
| ----- | ---------------------------------------------------------------------------- |
| bash  | `echo 'source <($HOME/.zircon/self/bin/zircon env)' >> ~/.bashrc; . .bashrc` |
| zsh   | `echo 'source <($HOME/.zircon/self/bin/zircon env)' >> ~/.zshrc; . .zshrc`   |

After that, you can install the latest Zirco toolchain with the following command:

```
$ zircon install nightly
✓ Successfully imported toolchain: nightly
✓ Set as current toolchain

To use this toolchain, run:
  source <(zircon env)
*** You may need to restart your shell or source your profile for changes to take effect. ***
```

Now, re-configure your shell environment to use the new toolchain:

```
$ source <(zircon env)
```

You're all done! You can verify that the Zirco compiler is installed and working by running:

```
$ zrc --version
zrc_cli 0.1.0 (commit ..., release build, ...)
```

## Install via Debian Package

Even on Debian-based systems, we recommend using `zircon` to install the Zirco toolchain, as it
allows you to easily upgrade, switch, and manage multiple versions of the Zirco toolchain. However,
if you prefer to install the Zirco toolchain via dpkg, you can download the latest Debian package
from the [releases page](https://github.com/zirco-lang/zrc/releases/tag/nightly).

```
$ dpkg -i zrc-0.1.0+1234567_amd64.deb
```

## Install via Nix

Zirco provides a Nix flake that provides the latest Zirco toolchain.

You can add our package to your flake inputs as follows:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    zirco-pkgs.url = "github:zirco-lang/zrc/main";
  };

  outputs = { self, nixpkgs, zirco-pkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          zirco-pkgs.packages.${system}.zrc
          zirco-pkgs.packages.${system}.libzr
        ];
      };
    };
}
```

> [!NOTE]
> System-wide Nix installations often lack required environment variables for the Zirco toolchain.
> If you encounter issues with missing libraries or include paths, you may need to manually set the
> `-I`/`-L` paths for headers and libzr.

## Download Prebuilt Binaries

Release tarballs are available on the [releases page](https://github.com/zirco-lang/zrc/releases/tag/nightly).

Ensure that `LD_LIBRARY_PATH` and `ZIRCO_INCLUDE_PATH` are set to the appropriate directories for the downloaded toolchain, and add the `bin` directory to your PATH.

## Build from Source

Instructions for building from source are available in the compiler's [README](https://github.com/zirco-lang/zrc)
file.
