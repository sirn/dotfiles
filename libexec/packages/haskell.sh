#!/bin/sh -e
#
# Install haskell packages.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')

HASKELL_PKGLIST=$LOOKUP_ROOT/var/bootstrap/pkglist_haskell.txt


## Preparation
##

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

    TMPDIR=/usr/local/cabal/build; export TMPDIR
}


## Run
##

_run() {
    if ! command -v cabal >/dev/null; then
        printe_h2 "Installing haskell cabal packages..."
        printe_info "cabal is not installed, skipping..."
        return
    fi

    if [ "$(command -v "_prepare_$PLATFORM")x" != "x" ]; then
        "_prepare_$PLATFORM"
    fi

    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    if version_gte "$(cabal --numeric-version)" 2.4.0.0; then
        cabal_prefix=v1-
    fi

    if is_force ||
            file_absent "$HOME/.cabal/packages/hackage.haskell.org"; then
        printe_h2 "Updating haskell cabal package index..."
        cabal "${cabal_prefix}update"
    fi

    for f in $(mangle_file "$HASKELL_PKGLIST" "$PLATFORM" "$FLAVORS"); do
        printe_h2 "Installing haskell cabal packages from $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            bin=${line%%:*}
            install=${line##$bin:}

            if is_force || file_absent "$HOME/.cabal/bin/$bin"; then
                # shellcheck disable=SC2086
                cabal "${cabal_prefix}install" $install
            fi
        done < "$f"
    done
}

_run
