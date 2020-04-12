#!/bin/sh -e
#
# Install rust packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../../share/bootstrap/utils.sh"

_preflight() {
    _setup_rust

    if ! command -v cargo >/dev/null; then
        printe_info "cargo is not installed, skipping rust packages..."
        return 1
    fi
}

_run_dev() {
    printe_h2 "Installing rust dev packages..."

    _rustup_install rls
    _rustup_install rust-analysis
    _rustup_install rust-src
    _cargo_install rustfmt rustfmt
}

_setup_rust() {
    PATH=$HOME/.cargo/bin:$PATH

    if ! forced && command -v rustup >/dev/null; then
        printe_info "rustup already installed, skipping..."
        return
    fi

    printe_h2 "Installing rust..."
    fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
}

_rustup_install() {
    PATH=$HOME/.cargo/bin:$PATH
    rustup component add "$@"
}

_cargo_install() {
    bin=$1; shift

    PATH=$HOME/.cargo/bin:$PATH
    pkgbin_path=$HOME/.cargo/bin/$bin

    if ! forced && [ -f "$pkgbin_path" ]; then
        printe_info "$pkgbin_path already exists, skipping..."
        return
    fi

    cargo install "$@"
}

run_with_flavors "$@"
