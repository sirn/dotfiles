#!/bin/sh -e
#
# Configure current user on Darwin.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

_setup_ipfs() {
    printe_h2 "Setting up ipfs..."

    ipfs_plist=$HOME/Library/LaunchAgents/io.ipfs.ipfs.plist

    if ! command -v ipfs >/dev/null; then
        printe_info "ipfs is not installed, skipping..."
        return
    fi

    if ! is_force && [ -f "$ipfs_plist" ]; then
        printe_info "$ipfs_plist already exists, skipping..."
        return
    fi

    cp \
        "$BASE_DIR/share/examples/launchd/io.ipfs.ipfs.plist" \
        "$ipfs_plist"

    chmod 0644 "$ipfs_plist"
    launchctl load -w "$ipfs_plist"
    printe_info "$ipfs_plist has been installed, you may need to relogin"
}

_run() {
    _setup_ipfs

    printe_h2 "Installing links..."
    make_link "$BASE_DIR/etc/aria2/aria2.conf" "$HOME/.aria2/aria2.conf"
    make_link \
        "$BASE_DIR/etc/emacs/straight/versions/default.el" \
        "$HOME/.emacs.d/straight/versions/default.el"

    make_link "$BASE_DIR/etc/emacs/init.el" "$HOME/.emacs.d/init.el"
    make_link "$BASE_DIR/etc/git/gitconfig" "$HOME/.gitconfig"
    make_link "$BASE_DIR/etc/hg/hgrc" "$HOME/.hgrc"
    make_link "$BASE_DIR/etc/ksh/kshrc" "$HOME/.kshrc"
    make_link "$BASE_DIR/etc/sh/profile" "$HOME/.profile"
    make_link "$BASE_DIR/etc/ssh/config" "$HOME/.ssh/config"
    make_link "$BASE_DIR/etc/tmux/tmux.conf" "$HOME/.tmux.conf"

    printe_h2 "Changing user shell..."
    change_shell oksh
}

_run_dev() {
    printe_h2 "Installing dev links..."
    make_link "$BASE_DIR/etc/proselint/proselintrc" "$HOME/.proselintrc"
}

run_with_flavors "$@"
