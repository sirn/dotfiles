#!/bin/sh -e
#
# Install elixir packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

# Ugly version parsing; we cannot directly sourcing erlang.sh since
# it would define _run_dev in here (which is never something we want)

if [ ! -f erlang.sh ]; then
    printe_err "erlang.sh not found"
    exit 1
fi

ERLANG_VERSION=$(awk '/ERLANG_VERSION=/ {
    sp=index($0, "=");
    print substr($0, sp + 1)
}' < "$(pwd -P)/erlang.sh")

ELIXIR_VERSION=1.11.4-otp-${ERLANG_VERSION%%.*}
ELIXIR_VERSION_PATH=$ASDF_DIR/installs/elixir/$ELIXIR_VERSION

ELIXIR_LS_VER=0.7.0
ELIXIR_LS_SHA256=d16cd5522907cfd36b4ed27e24913dd9c48e600db7719e92e3b41dd645f0322f

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping elixir..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing elixir..."
    _install_elixir
}

_install_elixir() {
    asdf_plugin elixir https://github.com/asdf-vm/asdf-elixir
    asdf_install elixir "$ELIXIR_VERSION" global
}

_run_dev() {
    _install_mix_hex
    _install_mix_rebar
    _install_elixir_ls
}

_install_mix_hex() {
    if ! forced && ls "$ELIXIR_VERSION_PATH/.mix/archives/hex-"* >/dev/null 2>&1; then
        return
    fi

    printe_info "mix hex is not installed, installing..."
    asdf_exec mix local.hex --force || exit 1
}

_install_mix_rebar() {
    if ! forced && [ -f "$ELIXIR_VERSION_PATH/.mix/rebar" ]; then
        return
    fi

    printe_info "mix rebar3 is not installed, installing..."
    asdf_exec mix local.rebar --force || exit 1
}

_install_elixir_ls() {
    _bindir=$ELIXIR_VERSION_PATH/bin
    _prefix=$ELIXIR_VERSION_PATH/opt/elixir_ls

    if ! forced && [ -L "$_bindir/elixir-language-server" ]; then
        printe_info "$_bindir/elixir-language-server already exists, skipping..."
        return
    fi

    printe_h2 "Installing elixir-ls..."

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive elixir-ls.tar.gz elixir-lsp/elixir-ls v$ELIXIR_LS_VER
    verify_shasum elixir-ls.tar.gz $ELIXIR_LS_SHA256
    run_tar -C "$BUILD_DIR" -xzf elixir-ls.tar.gz
    rm elixir-ls.tar.gz

    cd "$BUILD_DIR/elixir-ls-$ELIXIR_LS_VER" || exit 1
    install -d "$_prefix"

    MIX_ENV=prod mix "do" \
           deps.get, \
           compile, \
           elixir_ls.release -o "$_prefix"

    # Fix path so it could run in ELIXIR_VERSION_PREFIX
    sed "s|dir=.*|dir=$_prefix|" \
        "$_prefix/language_server.sh" > "$_prefix/language_server.sh.new"

    mv "$_prefix/language_server.sh.new" "$_prefix/language_server.sh"
    chmod 0755 "$_prefix/language_server.sh"
    make_link \
       "$_prefix/language_server.sh" \
       "$_bindir/elixir-language-server"

    printe_info "elixir_ls successfully installed"
    asdf_reshim elixir
}

run_with_flavors "$@"
