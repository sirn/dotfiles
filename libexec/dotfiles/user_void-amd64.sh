#!/bin/sh -e
#
# Configure current user on Void Linux.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_void.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_runit.sh"

_run_system() {
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
