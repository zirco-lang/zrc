#!/bin/sh
# Added to /etc/profile by the build-sysroot script.
# Similar to env.sh but...
# Does not set LD_LIBRARY_PATH because we use /usr/lib
# Does not set PATH to the toolchain bin because we use /usr/bin

export ZIRCO_INCLUDE_PATH="/usr/include/zirco:$ZIRCO_INCLUDE_PATH"
