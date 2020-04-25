#!/bin/sh -e
#
# Install Erlang-ls.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

REBAR3_VER=3.10.0
REBAR3_SHA256=5887a6228fec0a81d45416f53623563166d46b73b52638e6aaef6fa30d7ea5e7
REBAR3_HOME=$HOME/.cache/rebar3
REBAR3_PATH=$REBAR3_HOME/bin/rebar3

ERLANG_LS_VER=0.2.0
ERLANG_LS_SHA256=06e01d2d6d6afd1cb783ef9619d473b82ea9bec89c992a7e464a233ff7f1624e

_preflight() {
    if ! command -v escript >/dev/null; then
        printe_info "escript is not installed, skipping..."
        return 1
    fi
}

_run_dev() {
    _install_rebar3
    _install_erlang_ls
}

_install_rebar3() {
    printe_h2 "Installing rebar3..."

    if ! forced && [ -f "$REBAR3_PATH" ]; then
        printe_info "$REBAR3_PATH already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1
    fetch_gh_release rebar3 erlang/rebar3 $REBAR3_VER rebar3
    verify_shasum rebar3 $REBAR3_SHA256

    install -d "$(dirname "$REBAR3_PATH")"
    install -m0755 rebar3 "$REBAR3_PATH"
    escript "$REBAR3_PATH" local install
}

_install_erlang_ls() {
    printe_h2 "Installing erlang-ls..."

    if ! forced && [ -f "$HOME/.local/bin/erlang_ls" ]; then
        printe_info "$HOME/.local/bin/erlang_ls already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive erlang_ls.tar.gz erlang-ls/erlang_ls $ERLANG_LS_VER
    verify_shasum erlang_ls.tar.gz $ERLANG_LS_SHA256
    run_tar -C "$BUILD_DIR" -xzf erlang_ls.tar.gz
    rm erlang_ls.tar.gz

    cd "$BUILD_DIR/erlang_ls-$ERLANG_LS_VER" || exit 1
    make PATH="$(dirname "$REBAR3_PATH"):$PATH"
    install -d "$HOME/.local/bin"
    install -m0755 _build/default/bin/erlang_ls "$HOME/.local/bin/erlang_ls"
    printe_info "erlang_ls successfully installed"
}

run_with_flavors "$@"
