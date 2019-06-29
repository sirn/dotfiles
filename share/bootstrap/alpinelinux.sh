#!/bin/sh -e
#
# Shared functions for Alpine Linux
#

## Package installation
##

_check_installed() {
    pkg=$1; shift
    apk info -qe "$pkg"
}

_do_apk() {
    pkg=$1; shift

    if _check_installed "$pkg"; then
        printe "$pkg (apk) already installed"
        return
    fi

    printe "Installing $pkg (apk)..."
    run_root apk add "$pkg"
}
