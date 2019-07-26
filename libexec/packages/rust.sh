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

_setup_rust_src() {
    if ! command -v rustc >/dev/null; then
        return
    fi

    rust_ver=$(rustc --version | awk '{ print $2 }')
    rust_src_base=$HOME/.local/lib/rustlib/src/rust

    if ! forced && [ -d "$rust_src_base/rust-$rust_ver" ]; then
        printe_info "$rust_src_base/rust-$rust_ver already exists, skipping..."
        return
    fi

    printe_info "Preparing rust source code..."

    mkdir -p "$rust_src_base"
    fetch_gh_archive - "rust-lang/rust" "$rust_ver" |
        tar -C "$rust_src_base" -xzf -

    make_link "$rust_src_base/rust-$rust_ver/src" "$rust_src_base/src"
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
    _setup_rust_src

    if ! command -v cargo >/dev/null; then
        printe_info "cargo is not installed, skipping rust packages..."
        return 1
    fi
}

_run_dev() {
    printe_h2 "Installing rust dev packages..."

    # racer >= 2.1 requires nightly which we cannot use
    _cargo_install racer racer --vers ~2.0.0
    _cargo_install rustfmt rustfmt
}

run_with_flavors "$@"
