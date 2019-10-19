#!/bin/sh -e
#
# Shared functions for Void Linux
#

xbps_installed() {
    pkg=$1; shift
    xbps-query "$pkg" >/dev/null
}

xbps_install() {
    pkgs=

    for pkg in "$@"; do
        if xbps_installed "$pkg"; then
            printe "$pkg (xbps) already installed"
            continue
        fi

        pkgs="${pkgs:+$pkgs }$pkg"
    done

    if [ -n "$pkgs" ]; then
        eval set -- "$pkgs"
        run_root xbps-install -Sy "$@"
    fi
}
