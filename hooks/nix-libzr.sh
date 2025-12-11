# Nix setup hook for setting Zirco library paths for shells and builds that USE libzr

# HACK: I can't seem to otherwise find $out from within this setup hook.
# Get the directory of THIS script
HOOK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$HOOK_DIR/../libzr/lib"
export ZIRCO_INCLUDE_PATH="${ZIRCO_INCLUDE_PATH:-}:$HOOK_DIR/../libzr/include"
