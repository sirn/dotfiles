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
    pkgs=

    for pkg in "$@"; do
        if pkgng_installed "$pkg"; then
            printe "$pkg (pkgng) already installed"
        fi

        pkgs="${pkgs:+$pkgs }$pkg"
    done

    if [ -n "$pkgs" ]; then
        eval set -- "$pkgs"
        run_root pkg install -Uy "$@"
    fi
}
