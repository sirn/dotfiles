#!/bin/sh -e
#
# Install haskell packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

CABAL_PREFIX=

_preflight() {
    if ! command -v cabal >/dev/null; then
        printe_info "cabal is not installed, skipping haskell packages..."
        return 1
    fi

    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    if version_gte "$(cabal --numeric-version)" 2.4.0.0; then
        CABAL_PREFIX=v1-
    fi

    return 0
}

_run_dev() {
    _update_haskell_packages
}

_cabal_install() {
    bin=$1; shift

    if ! forced && [ -f "$HOME/.cabal/bin/$bin" ]; then
        printe_info "$HOME/.cabal/bin/$bin already exists, skipping..."
        return
    fi

    cabal "${CABAL_PREFIX}install" "$@"
}

_update_haskell_packages() {
    hackage_repo=$HOME/.cabal/packages/hackage.haskell.org

    if ! forced && [ -d "$hackage_repo" ]; then
        return
    fi

    printe_h2 "Updating haskell cabal package index..."
    cabal "${CABAL_PREFIX}update"
}

run_with_flavors "$@"
