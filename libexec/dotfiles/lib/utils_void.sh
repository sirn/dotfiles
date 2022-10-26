#!/bin/sh -e
#
# Shared functions for Void Linux
#

xbps_installed() {
    pkg=$1
    shift
    xbps-query "$pkg" >/dev/null
}

xbps_install() {
    pkg=$1
    shift

    if xbps_installed "$pkg"; then
        printe_info "$pkg (xbps) already installed"
        return
    fi

    printe_info "$pkg not installed, installing..."

    if [ -n "$pkg" ]; then
        eval set -- "$pkg"
        run_root xbps-install -Sy "$pkg"
    fi
}

xbps_current() {
    group=$1
    shift
    xbps-alternatives -l -g "$group" | awk '/(current)/ { print $2 }'
}

xbps_alternative() {
    group=$1
    shift
    pkg=$1
    shift

    if ! xbps_installed "$pkg"; then
        printe_info "$pkg is not installed, skipping..."
        return
    fi

    if [ "$(xbps_current "$group")" = "$pkg" ]; then
        printe_info "$pkg already the default $group"
        return
    fi

    run_root xbps-alternatives -s "$pkg" -g "$group"
}
