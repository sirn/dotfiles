#!/bin/sh -e
#
# Install Void Linux packages with XBPS (WSL variant).
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./pkg_void-amd64.sh"

_run_system() {
    : # Don't run system for WSL
}

_run_desktop() {
    xbps_install breeze
    xbps_install breeze-gtk
    xbps_install konsole
    xbps_install xrdb

    sh "$BASE_DIR/libexec/packages/sys/fonts.sh" "$@"
}

_run_all() {
    xbps_install emacs-x11
}
