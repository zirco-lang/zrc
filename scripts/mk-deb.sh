#!/usr/bin/env bash
# Build hook for the zrc compiler
# This script takes a Zirco sysroot produced by ./mk-sysroot.sh and makes a deb out of it.
#
# !!! NOTE !!! You must run ./build.sh, then ./mk-sysroot.sh, then this script in order to produce a deb.
#
# $SYSROOT is the sysroot built by ./mk-sysroot.sh, and the deb is built into $DEB_OUTPUT.
# CWD must be the repo root for this script to work.

set -e

if [ -z "$SYSROOT" ]; then
    echo "SYSROOT is not set"
    exit 1
fi

if [ -z "$DEB_OUTPUT" ]; then
    echo "DEB_OUTPUT is not set"
    exit 1
fi

# Get current git SHA
COMMIT=$(git rev-parse --short HEAD)
# Append -dirty if there are uncommitted changes
if [ -n "$(git status --porcelain)" ]; then
    COMMIT="$COMMIT-dirty"
fi

ARCH=$(dpkg --print-architecture)

PKG_DIR="zrc-0.1.0+${COMMIT}_$ARCH"

mkdir -p "$DEB_OUTPUT/$PKG_DIR/DEBIAN"
sed "s/{COMMIT}/$COMMIT/g; s/{ARCH}/$ARCH/g" dist/debian/control > "$DEB_OUTPUT/$PKG_DIR/DEBIAN/control"

cp -r "$SYSROOT"/* "$DEB_OUTPUT/$PKG_DIR/"

dpkg-deb --root-owner-group --build "$DEB_OUTPUT/$PKG_DIR"