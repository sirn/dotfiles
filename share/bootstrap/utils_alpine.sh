#!/bin/sh -e
#
# Shared functions for Alpine Linux
#

apk_installed() {
    pkg=$1; shift
    apk info -qe "$pkg"
}

apk_install() {
    pkg=$1; shift

    if apk_installed "$pkg"; then
        printe "$pkg (apk) already installed"
        return
    fi

    printe "Installing $pkg (apk)..."
    run_root apk add "$pkg"
}
