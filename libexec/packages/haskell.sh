#!/bin/sh -e
#
# Install haskell packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

CABAL_PREFIX=
PLATFORM=$(get_platform)

_prepare_openbsd() {
    printe_info "Preparing wxallowed for cabal..."

    # See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
    if [ ! -f /usr/local/cabal ]; then
        run_root mkdir -p /usr/local/cabal
        run_root chown "$USER:wheel" /usr/local/cabal
    fi

    if [ ! -f /usr/local/cabal/build ]; then
        run_root mkdir -p /usr/local/cabal/build
        run_root chown "$USER:wheel" /usr/local/cabal/build
    fi

    make_link /usr/local/cabal "$HOME/.cabal"

    TMPDIR=/usr/local/cabal/build
    export TMPDIR
}

_cabal_install() {
    bin=$1; shift

    if ! forced && [ -f "$HOME/.cabal/bin/$bin" ]; then
        printe_info "$HOME/.cabal/bin/$bin already exists, skipping..."
        return
    fi

    cabal "${CABAL_PREFIX}install" "$@"
}

_run() {
    printe_h2 "Installing haskell packages..."

    if ! command -v cabal >/dev/null; then
        printe_info "cabal is not installed, skipping haskell packages..."
        return 1
    fi

    if [ "$(command -v "_prepare_$PLATFORM")x" != "x" ]; then
        "_prepare_$PLATFORM"
    fi

    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    if version_gte "$(cabal --numeric-version)" 2.4.0.0; then
        CABAL_PREFIX=v1-
    fi

    hackage_repo=$HOME/.cabal/packages/hackage.haskell.org

    if ! forced && [ -d "$hackage_repo" ]; then
        printe_info "$hackage_repo already exists, skipping..."
        return
    fi

    printe_h2 "Updating haskell cabal package index..."
    cabal "${CABAL_PREFIX}update"
}

_run_dev() {
    case $PLATFORM in
        openbsd )
            printe_h2 "Installing haskell cabal dev packages..."

            _cabal_install shellcheck ShellCheck
            _cabal_install pandoc pandoc
            ;;

        * )
            ;;
    esac
}

run_with_flavors "$@"
