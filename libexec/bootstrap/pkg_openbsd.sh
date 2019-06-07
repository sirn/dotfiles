#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required
ensure_platform "OpenBSD"

FLAVORS=$*
PKG_PKGLIST=$LOOKUP_ROOT/var/bootstrap/openbsd/pkglist.txt


## Utils
##

_check_installed() {
    spec=$1; shift

    spec=${spec%%--*}   # pkg--flavor stem
    spec=${spec%%%*}    # pkg%version stem

    case "$spec" in
        *"-"*[0-9] ) ;; # probably a stem with version number
        * ) spec="$spec-*";;
    esac

    pkg_info -q -e "$spec"
}

_do_pkg_install() {
    pkg=$1; shift

    if _check_installed "$1"; then
        printe "$pkg (pkg) already installed"
        return
    fi

    printe "Installing $pkg (pkg)..."
    pkg install -y "$pkg"
}

_do_exec() {
    path=$1; shift
    "$LOOKUP_ROOT/$path" "$FLAVORS"
}


## Runs
##

_run() {
    for f in $(mangle_file "$PKG_PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            eval set -- "$line"

            case "$1" in
                pkg )   shift; _do_pkg_install "$@";;
                exec )  shift; _do_exec "$@";;
                * ) printe_err "Unknown directive: $1";;
            esac
        done
    done

    # Only install emacs--no_x11 when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs--gtk3)
    if ! _check_installed emacs; then
        run_root pkg_add emacs--no_x11
    fi

    # Only install w3m-- when other variant of w3m hasn't been installed
    # (e.g. desktop flavor installs w3m--image which is required by emacs-w3m
    # when running under GUI mode)
    if ! _check_installed w3m; then
        run_root pkg_add w3m--
    fi
}

# Restore Google Cloud state directory. This is required for kubectl
# to be able to authenticate with Google Cloud.
_run_dev() {
    if [ -d /usr/local/google-cloud-sdk ]; then
        run_root mkdir -p /usr/local/google-cloud-sdk/.install
    fi
}

run_with_flavors "$FLAVORS"
