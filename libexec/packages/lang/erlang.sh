#!/bin/sh -e
#
# Install erlang packages.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

ERLANG_VERSION=25.0.3
ERLANG_VERSION_PATH=$ASDF_DIR/installs/erlang/$ERLANG_VERSION

REBAR3_VER=3.19.0
REBAR3_SHA256=ff9ef42c737480477ebdf0dd9d95ece534a14c96f05edafbf32e9af973280bc3

REBAR3_HOME=$HOME/.cache/rebar3
REBAR3_PATH=$REBAR3_HOME/bin/rebar3

ERLANG_LS_VER=0.38.0
ERLANG_LS_SHA256=daa06a45063e9777aadf5ab7a0a677b8ba527ac5bff0dc49065834e3eb757778

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping erlang..."
        return 1
    fi
}

_run() {
    _install_erlang
}

_install_erlang() {
    asdf_plugin erlang https://github.com/asdf-vm/asdf-erlang
    asdf_install erlang "$ERLANG_VERSION" global
}

_run_dev() {
    _install_rebar3
    _install_erlang_ls
}

_install_rebar3() {
    # Rebar3 is independent of Erlang version install and extracts to ~/.cache
    # We're installing it outside of $ERLANG_VERSION_PATH here
    if ! forced && [ -f "$REBAR3_PATH" ]; then
        printe_info "$REBAR3_PATH already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1
    fetch_gh_release rebar3 erlang/rebar3 $REBAR3_VER rebar3
    verify_shasum rebar3 $REBAR3_SHA256

    install -d "$(dirname "$REBAR3_PATH")"
    install -m0755 rebar3 "$REBAR3_PATH"
    asdf_exec escript "$REBAR3_PATH" local install
}

_install_erlang_ls() {
    _bindir="$ERLANG_VERSION_PATH/bin"

    if ! forced && [ -f "$_bindir/erlang_ls" ]; then
        printe_info "$_bindir/erlang_ls already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive erlang_ls.tar.gz erlang-ls/erlang_ls $ERLANG_LS_VER
    verify_shasum erlang_ls.tar.gz $ERLANG_LS_SHA256
    run_tar -C "$BUILD_DIR" -xzf erlang_ls.tar.gz
    rm erlang_ls.tar.gz

    cd "$BUILD_DIR/erlang_ls-$ERLANG_LS_VER" || exit 1
    PATH="$ASDF_DIR/shims:$(dirname "$REBAR3_PATH"):$PATH"
    export PATH
    make
    install -d "$_bindir"
    install -m0755 _build/default/bin/erlang_ls "$_bindir/erlang_ls"
    printe_info "erlang_ls successfully installed"

    asdf_reshim erlang
}

run_with_flavors "$@"
