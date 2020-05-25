#!/bin/sh -e
#
# Install Void Linux packages with XBPS (WSL variant).
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./pkg_void.sh"

_run_system() {
    : # Don't run system for WSL
}

_run_desktop() {
    sh "$BASE_DIR/var/dotfiles/packages/sys/fonts.sh" "$@"
}

_run_all() {
    xbps_install emacs-x11
}
