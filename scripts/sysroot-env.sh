#!/bin/sh
# Added to /etc/profile by the build-sysroot script.
# Similar to env.sh but...
# Does not set LD_LIBRARY_PATH because we use /usr/lib
# Does not set PATH to the toolchain bin because we use /usr/bin

export ZIRCO_INCLUDE_PATH="/usr/include:$ZIRCO_INCLUDE_PATH"
export C_INCLUDE_PATH="/usr/include:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="/usr/include:$CPLUS_INCLUDE_PATH"
export LD_LIBRARY_PATH="/usr/lib:$LD_LIBRARY_PATH"
export DYLD_LIBRARY_PATH="/usr/lib:$DYLD_LIBRARY_PATH"