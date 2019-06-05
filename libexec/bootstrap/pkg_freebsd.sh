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


## Installs
##

_run() {
    if [ ! -x /usr/local/sbin/pkg ]; then
        printe_h2 "Bootstrapping pkgng..."
        run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
    fi

    for f in $(mangle_file "$PKG_PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."
        run_root xargs pkg install -y < "$f"
    done

    if [ "$BOOTSTRAP_ROOT" = "$LOOKUP_ROOT" ]; then
        "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_asdf.sh" "$FLAVORS"
        "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_local.sh" "$FLAVORS"
    fi
}

run_with_flavors "$FLAVORS"
