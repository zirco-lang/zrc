#!/usr/bin/env bash
# Build hook for the zrc compiler
# This script is used to build a sysroot in any folder containing the Zirco artifacts.
# ZIRCON_TOOLCHAIN_DIR is the source Zircon directory, and the sysroot is built into $SYSROOT.
# We install the binary, include files, etc into /usr and apply env.sh into /etc/profile.d for the sysroot.
# CWD must be the repo root for this script to work.

set -e

if [ -z "$ZIRCON_TOOLCHAIN_DIR" ]; then
    echo "ZIRCON_TOOLCHAIN_DIR is not set"
    exit 1
fi

if [ -z "$SYSROOT" ]; then
    echo "SYSROOT is not set"
    exit 1
fi

mkdir -p "$SYSROOT/usr/bin"
mkdir -p "$SYSROOT/usr/include/zirco"
mkdir -p "$SYSROOT/usr/lib"
mkdir -p "$SYSROOT/etc/profile.d"

cp "$ZIRCON_TOOLCHAIN_DIR/bin/"* "$SYSROOT/usr/bin/"
cp -r "$ZIRCON_TOOLCHAIN_DIR/include/"* "$SYSROOT/usr/include/zirco/"
cp -r "$ZIRCON_TOOLCHAIN_DIR/libzr/lib/"* "$SYSROOT/usr/lib/"
cp -r "$ZIRCON_TOOLCHAIN_DIR/libzr/include/"* "$SYSROOT/usr/include/zirco/"
cp "./scripts/sysroot-env.sh" "$SYSROOT/etc/profile.d/zircon-env.sh"

# Directories need these perms to be usable
find "$SYSROOT/usr/bin" "$SYSROOT/usr/include/zirco" "$SYSROOT/usr/lib" "$SYSROOT/etc/profile.d" -type d -exec chmod 755 {} \;

# Executable binaries
find "$SYSROOT/usr/bin" -type f -exec chmod 755 {} \;

# Headers
find "$SYSROOT/usr/include/zirco" -type f -exec chmod 644 {} \;

# Libraries
find "$SYSROOT/usr/lib" -type f -exec chmod 755 {} \;

# Profile scripts
chmod 755 "$SYSROOT/etc/profile.d/zircon-env.sh"
