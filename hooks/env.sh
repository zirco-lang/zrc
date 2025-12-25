#!/bin/sh
# Added to the resulting tarballs and sourced by `zircon env`

export PATH="$HOME/.zircon/toolchains/current/bin:\$PATH"
export ZIRCO_INCLUDE_PATH="$HOME/.zircon/toolchains/current/include:\$ZIRCO_INCLUDE_PATH"

# libzr
export LD_LIBRARY_PATH="$HOME/.zircon/toolchains/current/libzr/lib:\$LD_LIBRARY_PATH"
export ZIRCO_INCLUDE_PATH="$HOME/.zircon/toolchains/current/libzr/include:\$ZIRCO_INCLUDE_PATH"