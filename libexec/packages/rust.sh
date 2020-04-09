#!/bin/sh -e
#
# Install rust packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

_preflight() {
    if ! command -v rustc >/dev/null; then
        # Technically we shouldn't install things in preflight but this case
        # is a bit special since we're not installing Rust via pkgs...
        printe_h2 "Installing rust..."
        _setup_rust
    fi

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

run_with_flavors "$@"
