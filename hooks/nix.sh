# Nix setup hook for setting Zirco library paths for shells and builds that USE zrc

# HACK: I can't seem to otherwise find $out from within this setup hook.
# Get the directory of THIS script
HOOK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Assume include/ is positioned relative to the hook
LIB_DIR="$HOOK_DIR/../include"

# Now, we can finally inject into the environment!
export ZIRCO_INCLUDE_PATH="${ZIRCO_INCLUDE_PATH:-}:$LIB_DIR"
