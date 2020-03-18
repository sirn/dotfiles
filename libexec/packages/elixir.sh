#!/bin/sh -e
#
# Install Elixir-ls.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

ELIXIR_LS_VER=0.3.1
ELIXIR_LS_SHA256=56a72d5bd41c6676cd34a7595dfab46328098f8c7979114a3d75cc7237458d1a

_run() {
    printe_h2 "Installing elixir-ls..."

    _elixir_ls_bin=$HOME/.local/bin/elixir-language-server
    _elixir_ls_prefix=$HOME/.local/lib/elixir-ls

    if ! forced && [ -L $_elixir_ls_bin ]; then
        printe_info "$_elixir_ls_bin already exists, skipping..."
        return
    fi

    if ! command -v iex >/dev/null; then
        printe_info "elixir is not installed, skipping..."
        return
    fi

    if ! ls "$HOME/.local/share/mix/archives/hex-"* >/dev/null 2>&1; then
        printe_info "mix hex is not installed, installing..."
        mix local.hex --force || exit 1
    fi

    if [ ! -f "$HOME/.local/share/mix/rebar" ]; then
        printe_info "mix rebar3 is not installed, installing..."
        mix local.rebar --force || exit 1
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive elixir-ls.tar.gz elixir-lsp/elixir-ls v$ELIXIR_LS_VER
    verify_shasum elixir-ls.tar.gz $ELIXIR_LS_SHA256
    tar -C "$BUILD_DIR" -xzf elixir-ls.tar.gz
    rm elixir-ls.tar.gz

    cd "$BUILD_DIR/elixir-ls-$ELIXIR_LS_VER" || exit 1
    mkdir -p "$HOME/.local/lib/elixir-ls"

    MIX_ENV=prod mix "do" \
           deps.get, \
           compile, \
           elixir_ls.release -o "$_elixir_ls_prefix"

    ln -sf \
       "$_elixir_ls_prefix/language_server.sh" \
       "$HOME/.local/bin/elixir-language-server"

    printe_info "elixir_ls successfully installed"
}

_run
