#!/bin/sh -e
#
# Configure current user on OpenBSD.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

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
    change_shell ksh
}

_run_desktop() {
    printe_h2 "Installing desktop links..."
    make_link "$BASE_DIR/etc/cwm/cwmrc" "$HOME/.cwmrc"
    make_link "$BASE_DIR/etc/desktop/Xresources" "$HOME/.Xresources"
    make_link "$BASE_DIR/etc/desktop/xsession" "$HOME/.xsession"
    make_link "$BASE_DIR/etc/desktop/fonts.conf" "$HOME/.fonts.conf"
}

_run_dev() {
    printe_h2 "Installing dev links..."
    make_link "$BASE_DIR/etc/proselint/proselintrc" "$HOME/.proselintrc"
}

run_with_flavors "$@"
