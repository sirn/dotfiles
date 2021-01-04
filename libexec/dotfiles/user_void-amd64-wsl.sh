#!/bin/sh -e
#
# Configure current user on Void Linux (WSL variant).
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./user_void-amd64.sh"

_run() {
    _setup_user_links
    _setup_user_links_wsl
    _setup_user_shell
}

_setup_user_links_wsl() {
    printe_h2 "Installing WSL links..."

    make_link "$BASE_DIR/etc/wsl/profile2" "$HOME/.profile2"
    make_link "$BASE_DIR/etc/wsl/Xresources" "$HOME/.Xresources"
}
