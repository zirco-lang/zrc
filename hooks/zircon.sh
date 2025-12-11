#!/bin/sh
# Zircon installation hook for the zrc compiler
# This script is invoked by Zircon to compile the zrc source files.
# CWD is the repo root, and ZIRCON_TOOLCHAIN_DIR is the destination.
# We install the binary, include files, and bin.sh which is hooked by zircon env.

set -e

ZIRCON_BIN_DIR="$ZIRCON_TOOLCHAIN_DIR/bin"
ZIRCON_INCLUDE_DIR="$ZIRCON_TOOLCHAIN_DIR/include/zircon"

mkdir -p "$ZIRCON_BIN_DIR"
mkdir -p "$ZIRCON_INCLUDE_DIR"

cargo build --release

cp target/release/zrc "$ZIRCON_BIN_DIR/"
cp target/release/zircop "$ZIRCON_BIN_DIR/"

cp -r include "$ZIRCON_INCLUDE_DIR/"

cat > "$ZIRCON_BIN_DIR/bin.sh" << EOF
#!/bin/sh
export PATH="$ZIRCON_BIN_DIR:\$PATH"
export ZIRCO_INCLUDE_PATH="$ZIRCON_INCLUDE_DIR:\$ZIRCO_INCLUDE_PATH"
EOF

chmod +x "$ZIRCON_BIN_DIR/bin.sh"
