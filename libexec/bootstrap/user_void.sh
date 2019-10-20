#!/bin/sh -e
#
# Configure current user on Void Linux.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_void.sh"

_run() {
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

    change_shell loksh
}

_run_desktop() {
    printe_h2 "Installing desktop configurations..."

    fontconf_conf=$HOME/.config/fontconfig/fonts.conf

    if ! forced && [ -f "$fontconf_conf" ]; then
        printe_info "$fontconf_conf already exists, skipping..."
        return
    fi

    cp "$BASE_DIR/etc/desktop/fonts.conf.in" "$fontconf_conf"
    printe_info "$fontconf_conf successfully created"
}

_run_dev() {
    printe_h2 "Installing dev links..."

    make_link "$BASE_DIR/etc/proselint/proselintrc" "$HOME/.proselintrc"
}

run_with_flavors "$@"
