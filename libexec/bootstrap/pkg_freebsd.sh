#!/bin/sh -e
#
# Install FreeBSD packages with Pkgng.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required
ensure_platform "FreeBSD"

FLAVORS=$*
PKG_PKGLIST=$LOOKUP_ROOT/var/bootstrap/freebsd/pkglist.txt


## Utils
##

_do_pkgng_install() {
    pkg=$1; shift
    pkg install -y "$pkg"
}

_do_exec() {
    path=$1; shift
    "$LOOKUP_ROOT/$path" "$FLAVORS"
}


## Setup
##

_setup_env() {
    if [ ! -x /usr/local/sbin/pkg ]; then
        printe_h2 "Bootstrapping pkgng..."
        run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
    fi
}


## Runs
##

_run() {
    _setup_env

    for f in $(mangle_file "$PKG_PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            eval set -- "$line"

            case "$1" in
                pkgng ) shift; _do_pkgng_install "$@";;
                exec )  shift; _do_exec "$@";;
                * ) printe_err "Unknown directive: $1";;
            esac
        done < "$f"
    done
}

run_with_flavors "$FLAVORS"
