#!/bin/sh
# Zircon installation hook for the zrc compiler
# This script is invoked by Zircon to compile the zrc source files.
# CWD is the repo root, and ZIRCON_TOOLCHAIN_DIR is the destination.
# We install the binary, include files, and env.sh which is hooked by zircon env.

set -e

ZIRCON_BIN_DIR="$ZIRCON_TOOLCHAIN_DIR/bin"
ZIRCON_INCLUDE_DIR="$ZIRCON_TOOLCHAIN_DIR/include"

mkdir -p "$ZIRCON_BIN_DIR"
mkdir -p "$ZIRCON_INCLUDE_DIR"

cargo build --release

cp target/release/zrc "$ZIRCON_BIN_DIR/"
cp target/release/zircop "$ZIRCON_BIN_DIR/"

cp -r include/* "$ZIRCON_INCLUDE_DIR/"

cat > "$ZIRCON_TOOLCHAIN_DIR/env.sh" << EOF
#!/bin/sh
export PATH="$HOME/.zircon/toolchains/current/bin:\$PATH"
export ZIRCO_INCLUDE_PATH="$HOME/.zircon/toolchains/current/include:\$ZIRCO_INCLUDE_PATH"
EOF

chmod +x "$ZIRCON_TOOLCHAIN_DIR/env.sh"
