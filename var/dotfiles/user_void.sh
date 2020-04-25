#!/bin/sh -e
#
# Configure current user on Void Linux.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_void.sh"
. "lib/utils_runit.sh"
. "lib/buildenv.sh"

_run() {
    _setup_user_service
    _setup_user_links
    _setup_user_shell
    _setup_user_emacs
}

_run_desktop() {
    _setup_desktop_links
}

_run_dev() {
    _setup_dev_links
}

_setup_user_service() {
    printe_h2 "Setting up user service..."

    if [ -z "$USER" ]; then
        printe_info "cannot determine current user, skipping..."
        return
    fi

    svcname=runsvdir-$USER
    svcsrc=/etc/sv/$svcname

    if [ ! -f "$svcsrc/run" ]; then
        cd "$BUILD_DIR" || exit 1

        cat <<EOF > "$BUILD_DIR/${svcname}_run"
#!/bin/sh

if [ ! -d /run/runit.$USER ]; then
    install -d -o$USER -g$USER /run/runit.$USER
fi

HOME=$HOME; export HOME

exec 2>&1
exec chpst -u "$USER:\$(id -Gn $USER | tr ' ' ':')" runsvdir $HOME/.local/var/service 'log: ...........................................................................................................................................................................................................................................................................................................................................................................................................'
EOF

        run_root install -d "$svcsrc"
        run_root install -m0755 "$BUILD_DIR/${svcname}_run" "$svcsrc/run"
    fi

    if [ ! -f "$svcsrc/finish" ]; then
        cd "$BUILD_DIR" || exit 1

        cat <<EOF > "$BUILD_DIR/${svcname}_finish"
#!/bin/sh

sv -w600 force-stop $HOME/.local/var/service/*
sv exit $HOME/.local/var/service/*
EOF

        run_root install -d "$svcsrc"
        run_root install -m0755 "$BUILD_DIR/${svcname}_finish" "$svcsrc/finish"
    fi

    install_svc -S "$svcsrc"
}

_setup_user_emacs() {
    printe_h2 "Installing emacs service..."

    install_svc -u -s -p emacs "$BASE_DIR/sv/emacs"
}

_setup_user_links() {
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
}

_setup_user_shell() {
    printe_h2 "Changing user shell..."

    change_shell loksh
}

_setup_desktop_links() {
    printe_h2 "Installing desktop links..."

    make_link \
        "$BASE_DIR/etc/fontconfig/conf.d" \
        "$HOME/.config/fontconfig/conf.d"
}

_setup_dev_links() {
    printe_h2 "Installing dev links..."

    make_link "$BASE_DIR/etc/proselint/proselintrc" "$HOME/.proselintrc"
}

run_with_flavors "$@"
