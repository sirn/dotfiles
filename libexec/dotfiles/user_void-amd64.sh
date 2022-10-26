#!/bin/sh -e
#
# Configure current user on Void Linux.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC1091
. "lib/utils.sh"

# shellcheck disable=SC1091
. "lib/utils_void.sh"

# shellcheck disable=SC1091
. "lib/utils_runit.sh"

_run() {
    _setup_user_service
    _setup_user_shell
}

_run_desktop() {
    _setup_desktop_links
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
        run_root install -d "$svcsrc"
        run_root install -m0755 "$BASE_DIR/etc/sv/runsvdir/run" "$svcsrc/run"
        make_link -Sf "/run/runit/supervise.$svcname" "$svcsrc/supervise"
    fi

    if [ ! -f "$svcsrc/finish" ]; then
        run_root install -d "$svcsrc"
        run_root install -m0755 "$BASE_DIR/etc/sv/runsvdir/finish" "$svcsrc/finish"
    fi

    install_svc -S "$svcsrc"
}

_setup_user_shell() {
    printe_h2 "Changing user shell..."

    update_shells oksh
    change_shell oksh
}

_setup_desktop_links() {
    make_link \
        "$BASE_DIR/etc/fontconfig/conf.d" \
        "$HOME/.config/fontconfig/conf.d"
}
