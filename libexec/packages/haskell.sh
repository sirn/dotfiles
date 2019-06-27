#!/bin/sh -e
#
# Install haskell packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

CABAL_PREFIX=
PLATFORM=$(get_platform)

_prepare_openbsd() {
    printe_h2 "Preparing wxallowed for cabal..."

    # See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
    if file_absent /usr/local/cabal; then
        run_root mkdir -p /usr/local/cabal
        run_root chown "$USER:wheel" /usr/local/cabal
    fi

    if file_absent /usr/local/cabal/build; then
        run_root mkdir -p /usr/local/cabal/build
        run_root chown "$USER:wheel" /usr/local/cabal/build
    fi

    make_link /usr/local/cabal "$HOME/.cabal"

    TMPDIR=/usr/local/cabal/build
    export TMPDIR
}

_do_cabal_install() {
    bin=$1; shift

    if is_force || file_absent "$HOME/.cabal/bin/$bin"; then
        cabal "${CABAL_PREFIX}install" "$@"
    fi
}

_run() {
    if ! command -v cabal >/dev/null; then
        printe_h2 "Installing haskell cabal packages..."
        printe_info "cabal is not installed, skipping..."
        return 1
    fi

    if [ "$(command -v "_prepare_$PLATFORM")x" != "x" ]; then
        "_prepare_$PLATFORM"
    fi

    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    if version_gte "$(cabal --numeric-version)" 2.4.0.0; then
        CABAL_PREFIX=v1-
    fi

    if is_force || file_absent "$HOME/.cabal/packages/hackage.haskell.org"; then
        printe_h2 "Updating haskell cabal package index..."
        cabal "${CABAL_PREFIX}update"
    fi
}

_run_dev() {
    case $PLATFORM in
        openbsd )
            printe_h2 "Installing haskell cabal dev packages..."
            _do_cabal_install shellcheck ShellCheck
            _do_cabal_install pandoc pandoc
            ;;

        * )
            ;;
    esac
}

run_with_flavors "$@"
