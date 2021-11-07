#!/bin/sh -e
#
# Install rust packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

RUST_VERSION=1.56.1
RUST_VERSION_PATH=$ASDF_DIR/installs/rust/$RUST_VERSION

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping rust..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing rust..."
    _install_rust
}

_install_rust() {
    RUSTUP_INIT_SKIP_PATH_CHECK=yes; export RUSTUP_INIT_SKIP_PATH_CHECK
    asdf_plugin rust https://github.com/code-lever/asdf-rust
    asdf_install rust "$RUST_VERSION" global
}

_run_dev() {
    printe_h2 "Installing rust dev packages..."

    _rustup_install rls
    _rustup_install rust-analysis
    _rustup_install rust-src
    _cargo_install rustfmt rustfmt
    _cargo_install mdbook mdbook
}

_rustup_install() {
    asdf_exec rustup component add "$@"
}

_cargo_install() {
    bin=$1; shift

    pkgbin_path=$RUST_VERSION_PATH/bin/$bin

    if ! forced && [ -f "$pkgbin_path" ]; then
        printe_info "$pkgbin_path already exists, skipping..."
        return
    fi

    asdf_exec cargo install "$@"
}

run_with_flavors "$@"
