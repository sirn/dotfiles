#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

## Package installation
##

_check_installed() {
    spec=$1; shift

    spec=${spec%%--*}    # pkg--flavor stem
    spec=${spec%%%*}     # pkg%version stem

    case "$spec" in
        *"-"[0-9]* ) ;;  # probably a stem with version number
        * ) spec="$spec-*";;
    esac

    pkg_info -q -e "$spec"
}

_do_pkg() {
    pkg=$1; shift

    if _check_installed "$pkg"; then
        printe "$pkg (pkg) already installed"
        return
    fi

    printe "Installing $pkg (pkg)..."
    run_root pkg_add "$pkg"
}
