#!/bin/sh -e
#
# Shared functions for FreeBSD
#

## Package installation
##

_check_installed() {
    pkg=$1; shift
    pkg info -q "$pkg"
}

_do_pkgng() {
    pkg=$1; shift

    if _check_installed "$pkg"; then
        printe "$pkg (pkgng) already installed"
        return
    fi

    printe "Installing $pkg (pkg)..."
    run_root pkg install -Uy "$pkg"
}
