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


## Installs
##

_run() {
    PKG_PKGLIST=$LOOKUP_ROOT/var/bootstrap/openbsd/pkglist.txt

    for f in $(mangle_file "$PKG_PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."
        run_root xargs pkg_add < "$f"
    done

    # Only install emacs--no_x11 when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs--gtk3)
    if ! pkg_info -q -e "emacs-*"; then
        run_root pkg_add emacs--no_x11
    fi

    # Only install w3m-- when other variant of w3m hasn't been installed
    # (e.g. desktop flavor installs w3m--image which is required by emacs-w3m
    # when running under GUI mode)
    if ! pkg_info -q -e "w3m-*"; then
        run_root pkg_add w3m--
    fi

    "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_asdf.sh" "$FLAVORS"

    if [ "$BOOTSTRAP_ROOT" = "$LOOKUP_ROOT" ]; then
        "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_local.sh" "$FLAVORS"
    fi
}

_run_dev() {
    if [ "$BOOTSTRAP_ROOT" = "$LOOKUP_ROOT" ]; then
        # Restore Google Cloud state directory. This is required for kubectl
        # to be able to authenticate with Google Cloud.
        run_root mkdir -p /usr/local/google-cloud-sdk/.install

        # Packages not available under OpenBSD Ports
        "$BOOTSTRAP_ROOT/libexec/packages/execline.sh" "$FLAVORS"
        "$BOOTSTRAP_ROOT/libexec/packages/git-crypt.sh" "$FLAVORS"
        "$BOOTSTRAP_ROOT/libexec/packages/leiningen.sh" "$FLAVORS"
    fi
}

run_with_flavors "$FLAVORS"
