#!/usr/bin/env bash
# Run by build.sh after binaries are built to run some PGO profiling.

if [ -z "$BIN" ]; then
    echo "BIN must be set to the directory containing the built binaries"
    exit 1
fi

if [ "$BOLT" = "1" ]; then
    BOLT_SUFFIX="-bolt-instrumented"
else
    BOLT_SUFFIX=""
fi

LLVM_PROFILE_FILE="$(realpath ./target/pgo-profiles/zrc_%m_%p.profraw)" make -C examples clean zrc-only ZRC="$(realpath $BIN/zrc$BOLT_SUFFIX)"

LLVM_PROFILE_FILE="$(realpath ./target/pgo-profiles/zircop_%m_%p.profraw)" make -C examples lint ZIRCOP="$(realpath $BIN/zircop$BOLT_SUFFIX)"
