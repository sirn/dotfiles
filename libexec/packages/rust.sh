#!/bin/sh -e
#
# Install rust packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

_setup_rust() {
    PATH=$HOME/.cargo/bin:$PATH
    rustup_path=$HOME/.cargo/bin/rustup

    if ! forced && [ -f "$rustup_path" ]; then
        printe_info "$rustup_path already exists, skipping..."
        return
    fi

    fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
}

_rustup_install() {
    PATH=$HOME/.cargo/bin:$PATH
    rustup_path=$HOME/.cargo/bin/rustup

    $rustup_path component add "$@"
}

_cargo_install() {
    bin=$1; shift

    if ! forced && [ -f "$HOME/.cargo/bin/$bin" ]; then
        printe_info "$HOME/.cargo/bin/$bin already exists, skipping..."
        return
    fi

    cargo install "$@"
}

_run() {
    printe_h2 "Installing rust..."

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

run_with_flavors "$@"
