#!/usr/bin/env bash
# Build hook for the zrc compiler
# This script is invoked by a github action to compile the zrc source files.
# CWD is the repo root, and ZIRCON_TOOLCHAIN_DIR is the destination.
# We install the binary, include files, and env.sh which is hooked by zircon env into the toolchain
# directory in the proper format for Zircon.

set -e

mkdir -p "$ZIRCON_TOOLCHAIN_DIR/bin"
mkdir -p "$ZIRCON_TOOLCHAIN_DIR/include"
mkdir -p "$ZIRCON_TOOLCHAIN_DIR/include/zirco"
mkdir -p "$ZIRCON_TOOLCHAIN_DIR/lib"
mkdir -p "$ZIRCON_TOOLCHAIN_DIR/libzr/lib"
mkdir -p "$ZIRCON_TOOLCHAIN_DIR/libzr/include"

cargo build --release

cp target/release/zrc "$ZIRCON_TOOLCHAIN_DIR/bin"
cp target/release/zircop "$ZIRCON_TOOLCHAIN_DIR/bin"
cp target/release/zrx "$ZIRCON_TOOLCHAIN_DIR/bin"
cp target/release/zrepl "$ZIRCON_TOOLCHAIN_DIR/bin"
cp -r include/* "$ZIRCON_TOOLCHAIN_DIR/include/zirco/"
cp target/release/libzrc.a "$ZIRCON_TOOLCHAIN_DIR/lib/"
find target/release -maxdepth 1 -type f \( -name "libzrc.so" -o -name "libzrc.dylib" \) -exec cp {} "$ZIRCON_TOOLCHAIN_DIR/lib/" \;
cp compiler/zrc_c/zrc.h "$ZIRCON_TOOLCHAIN_DIR/include/"

# build std using the fresh compiler
make -C libzr all-opt ZRC=$ZIRCON_TOOLCHAIN_DIR/bin/zrc

cp libzr/dist/libzr.a "$ZIRCON_TOOLCHAIN_DIR/libzr/lib/"
find libzr/dist -maxdepth 1 -type f \( -name "libzr.so" -o -name "libzr.dylib" \) -exec cp {} "$ZIRCON_TOOLCHAIN_DIR/libzr/lib/" \;
cp -r libzr/include/* "$ZIRCON_TOOLCHAIN_DIR/libzr/include/"

cp scripts/env.sh "$ZIRCON_TOOLCHAIN_DIR/env.sh"

chmod +x "$ZIRCON_TOOLCHAIN_DIR/env.sh"
