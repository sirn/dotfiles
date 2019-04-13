#!/bin/sh -e
#
# Install haskell packages.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Preparation
##

case $platform in
    openbsd )
        if command -v cabal >/dev/null; then
            printe_h2 "Preparing wxallowed for cabal..."

            # See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
            if file_absent /usr/local/cabal; then
                run_root mkdir -p /usr/local/cabal
                run_root mkdir -p /usr/local/cabal/build
                run_root chown "$USER:wheel" /usr/local/cabal
                run_root chown "$USER:wheel" /usr/local/cabal/build
            fi

            make_link /usr/local/cabal "$HOME/.cabal"

            TMPDIR=/usr/local/cabal/build; export TMPDIR
        fi
        ;;

    * )
        ;;
esac


## Setup
##

haskell_pkglist=$lookup_dir/var/bootstrap/pkglist_haskell.txt

if command -v cabal >/dev/null; then
    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    if version_gte "$(cabal --numeric-version)" 2.4.0.0; then
        cabal_prefix=v1-
    fi

    if is_force || file_absent "$HOME/.cabal/packages/hackage.haskell.org"; then
        printe_h2 "Updating haskell cabal package index..."
        cabal "${cabal_prefix}update"
    fi

    for f in $(mangle_file "$haskell_pkglist" "$platform" "$flavors"); do
        printe_h2 "Installing haskell cabal packages from ${f##$lookup_dir/}..."

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
fi
