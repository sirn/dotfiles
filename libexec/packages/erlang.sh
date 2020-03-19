#!/bin/sh -e
#
# Install Erlang-ls.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

ERLANG_LS_VER=0.2.0
ERLANG_LS_SHA256=06e01d2d6d6afd1cb783ef9619d473b82ea9bec89c992a7e464a233ff7f1624e

_run() {
    printe_h2 "Installing erlang-ls..."

    if ! forced && [ -f "$HOME/.local/bin/erlang_ls" ]; then
        printe_info "$HOME/.local/bin/erlang_ls already exists, skipping..."
        return
    fi

    if ! command -v escript >/dev/null; then
        printe_info "erlang is not installed, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive erlang_ls.tar.gz erlang-ls/erlang_ls $ERLANG_LS_VER
    verify_shasum erlang_ls.tar.gz $ERLANG_LS_SHA256
    tar -C "$BUILD_DIR" -xzf erlang_ls.tar.gz
    rm erlang_ls.tar.gz

    cd "$BUILD_DIR/erlang_ls-$ERLANG_LS_VER" || exit 1
    make
    mkdir -p "$HOME/.local/bin"
    install -m0755 _build/default/bin/erlang_ls "$HOME/.local/bin/erlang_ls"
    printe_info "erlang_ls successfully installed"
}

_run