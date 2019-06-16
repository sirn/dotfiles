#!/bin/sh -e
#
# Shared functions for Darwin
#

PLATFORM_VERS=$(sw_vers -productVersion)

MAS=/opt/local/bin/mas
MAS_PLATFORM=10.14

MACPORTS=/opt/local/bin/port


## Package installation
##

_do_macports() {
    pkg=$1; shift

    case "$($MACPORTS installed "$pkg" 2>&1)" in
        "None of the"* ) ;;
        * )
            printe "$pkg (macports) already installed"
            return
            ;;
    esac

    printe "Installing $pkg (macports)..."

    if ! run_root $MACPORTS -n install "$pkg" "$@"; then
        printe_info "$pkg (macports) failed to install, skipping..."
    fi
}

_do_mas() {
    pkg_id=$1; shift
    app_name=$*

    if [ -d "/Applications/$app_name.app" ]; then
        printe "$app_name (mas) already installed"
        return
    fi

    if ! version_gte "$MAS_PLATFORM" "$PLATFORM_VERS"; then
        printe "mas is not available, please manually install $app_name"
        return
    fi

    printe "Installing $app_name (mas)..."

    if ! "$MAS" install "$pkg_id"; then
        printe_info "$app_name (mas) failed to install, skipping..."
    fi
}
