#!/bin/sh -e
#
# Shared functions for FreeBSD
#

pkgng_installed() {
    pkg=$1; shift
    pkg info -q "$pkg"
}

pkgng_bootstrap() {
    if [ ! -x /usr/local/sbin/pkg ]; then
        printe_h2 "Bootstrapping pkgng..."
        run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
    else
        printe_h2 "Updating pkg..."
        run_root pkg update -q
    fi
}

pkgng_install() {
    pkg=$1; shift

    if pkgng_installed "$pkg"; then
        printe "$pkg (pkgng) already installed"
        return
    fi

    printe "Installing $pkg (pkg)..."
    run_root pkg install -Uy "$pkg"
}
