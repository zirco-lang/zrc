#!/usr/bin/env bash
# Build hook for the zrc compiler
# This script is invoked by a github action to compile the zrc source files.
# CWD is the repo root, and ZIRCON_TOOLCHAIN_DIR is the destination.
# We install the binary, include files, and env.sh which is hooked by zircon env into the toolchain
# directory in the proper format for Zircon.

set -e

TRIPLE="$(rustc -vV | grep host | cut -d ' ' -f 2)"

ZIRCON_BIN_DIR="$ZIRCON_TOOLCHAIN_DIR/bin"
ZIRCON_INCLUDE_DIR="$ZIRCON_TOOLCHAIN_DIR/include"
ZIRCON_LIB_DIR="$ZIRCON_TOOLCHAIN_DIR/lib"
ZIRCON_LIBZR_DIR="$ZIRCON_TOOLCHAIN_DIR/libzr"

mkdir -p "$ZIRCON_BIN_DIR"
mkdir -p "$ZIRCON_INCLUDE_DIR"
mkdir -p "$ZIRCON_LIB_DIR"
mkdir -p "$ZIRCON_LIBZR_DIR/lib"
mkdir -p "$ZIRCON_LIBZR_DIR/include"

if [ "$PGO" = "1" ]; then

    mkdir -p target/tools
    cargo install cargo-pgo --root target/tools
    target/tools/bin/cargo-pgo pgo instrument build
    target/tools/bin/cargo-pgo pgo instrument test
    BIN="target/$TRIPLE/release/" BOLT=0 ./scripts/pgo-profiling.sh

    if [ "$BOLT" = "1" ]; then
        target/tools/bin/cargo-pgo pgo bolt build --with-pgo
        BIN="target/$TRIPLE/release/" BOLT=1 ./scripts/pgo-profiling.sh
        target/tools/bin/cargo-pgo pgo bolt optimize --with-pgo
        BOLT_SUFFIX="-bolt-optimized"
    else
        target/tools/bin/cargo-pgo pgo optimize build
        BOLT_SUFFIX=""
    fi

    rm -f compiler/zrc_parser/default.profraw

    cp target/$TRIPLE/release/zrc$BOLT_SUFFIX "$ZIRCON_BIN_DIR/zrc"
    cp target/$TRIPLE/release/zircop$BOLT_SUFFIX "$ZIRCON_BIN_DIR/zircop"
    cp target/$TRIPLE/release/zrx$BOLT_SUFFIX "$ZIRCON_BIN_DIR/zrx"
    cp target/$TRIPLE/release/zrepl$BOLT_SUFFIX "$ZIRCON_BIN_DIR/zrepl"
    cp target/$TRIPLE/release/libzrc.a "$ZIRCON_LIB_DIR/"
    find target/$TRIPLE/release -maxdepth 1 -type f \( -name "libzrc.so" -o -name "libzrc.dylib" \) -exec cp {} "$ZIRCON_LIB_DIR/" \;

else

    cargo build --release

    cp target/release/zrc "$ZIRCON_BIN_DIR/"
    cp target/release/zircop "$ZIRCON_BIN_DIR/"
    cp target/release/zrx "$ZIRCON_BIN_DIR/"
    cp target/release/zrepl "$ZIRCON_BIN_DIR/"
    cp target/release/libzrc.a "$ZIRCON_LIB_DIR/"
    find target/release -maxdepth 1 -type f \( -name "libzrc.so" -o -name "libzrc.dylib" \) -exec cp {} "$ZIRCON_LIB_DIR/" \;

fi

cp -r include/* "$ZIRCON_INCLUDE_DIR/"
cp compiler/libzrc/zrc.h "$ZIRCON_INCLUDE_DIR/"

# build std using the fresh compiler
make -C libzr all-opt ZRC="$(realpath $ZIRCON_BIN_DIR/zrc)"

cp libzr/dist/libzr.a "$ZIRCON_LIBZR_DIR/lib/"
find libzr/dist -maxdepth 1 -type f \( -name "libzr.so" -o -name "libzr.dylib" \) -exec cp {} "$ZIRCON_LIBZR_DIR/lib/" \;
cp -r libzr/include/* "$ZIRCON_LIBZR_DIR/include/"

cp scripts/env.sh "$ZIRCON_TOOLCHAIN_DIR/env.sh"

chmod +x "$ZIRCON_TOOLCHAIN_DIR/env.sh"
