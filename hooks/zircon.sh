#!/bin/sh
# Zircon installation hook for the zrc compiler
# This script is invoked by Zircon to compile the zrc source files.
# CWD is the repo root, and ZIRCON_TOOLCHAIN_DIR is the destination.
# We install the binary, include files, and env.sh which is hooked by zircon env.

set -e

ZIRCON_BIN_DIR="$ZIRCON_TOOLCHAIN_DIR/bin"
ZIRCON_INCLUDE_DIR="$ZIRCON_TOOLCHAIN_DIR/include"
ZIRCON_LIBZR_DIR="$ZIRCON_TOOLCHAIN_DIR/libzr"

mkdir -p "$ZIRCON_BIN_DIR"
mkdir -p "$ZIRCON_INCLUDE_DIR"
mkdir -p "$ZIRCON_LIBZR_DIR/lib"
mkdir -p "$ZIRCON_LIBZR_DIR/include"

cargo build --release

cp target/release/zrc "$ZIRCON_BIN_DIR/"
cp target/release/zircop "$ZIRCON_BIN_DIR/"
cp -r include/* "$ZIRCON_INCLUDE_DIR/"

# build std using the fresh compiler
make -C libzr all-opt ZRC=$ZIRCON_BIN_DIR/zrc

cp libzr/dist/libzr.a "$ZIRCON_LIBZR_DIR/lib/"
cp libzr/dist/libzr.so "$ZIRCON_LIBZR_DIR/lib/"
cp -r libzr/include/* "$ZIRCON_LIBZR_DIR/include/"

cp hooks/env.sh "$ZIRCON_TOOLCHAIN_DIR/env.sh"

chmod +x "$ZIRCON_TOOLCHAIN_DIR/env.sh"
