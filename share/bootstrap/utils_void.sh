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

xbps_current() {
    group=$1; shift
    xbps-alternatives -l -g "$group" | awk '/(current)/ { print $2 }'
}

xbps_alternative() {
    group=$1; shift
    pkg=$1; shift

    if ! xbps_installed "$pkg"; then
        printe "$pkg is not installed, skipping..."
        return
    fi

    if [ "$(xbps_current "$group")" = "$pkg" ]; then
        printe "$pkg already the default $group"
        return
    fi

    run_root xbps-alternatives -s "$pkg" -g "$group"
}
