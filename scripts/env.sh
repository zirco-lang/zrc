#!/bin/sh
# Added to the resulting tarballs and sourced by `zircon env`

base="$HOME/.zircon/toolchains/current"

export PATH="$base/bin:$PATH"
export ZIRCO_INCLUDE_PATH="$base/include/zirco:$ZIRCO_INCLUDE_PATH"

# for libzrc (compiler APIs)
export LD_LIBRARY_PATH="$base/lib:$LD_LIBRARY_PATH"
export C_INCLUDE_PATH="$base/include:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$base/include:$CPLUS_INCLUDE_PATH"

# libzr
export LD_LIBRARY_PATH="$base/libzr/lib:$LD_LIBRARY_PATH"
export ZIRCO_INCLUDE_PATH="$base/libzr/include:$ZIRCO_INCLUDE_PATH"

unset base
