#!/bin/sh -e
#
# Install parinfer-rust.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

# Workaround for "Dynamic loading not supported" on Musl
case $(get_libc) in
    musl )
        RUSTFLAGS=-Ctarget-feature=-crt-static; export RUSTFLAGS
        ;;
esac

PARINFER_RUST_VERSION=0.4.3
PARINFER_RUST_SHA256=752fd8eaa8c0c314c69b6be3415fbc4103f5ef9d1ad01a8b792545bfaff2d201

_preflight() {
    if ! command -v cargo >/dev/null; then
       printe_h2 "cargo is not installed, skipping parinfer-rust..."
       return 1
    fi
}

_run() {
    printe_h2 "Installing parinfer-rust..."
    _install_parinfer_rust
}

_install_parinfer_rust() {
    _verdir=$HOME/.emacs.d/var/parinfer-rust/$PARINFER_RUST_VERSION
    _installdir=$HOME/.emacs.d/var/parinfer-rust

    case $(uname) in
        Darwin )
            _libname=parinfer-rust-darwin.so
            _buildname=libparinfer_rust.dylib
            ;;
        Linux  )
            _libname=parinfer-rust-linux.so
            _buildname=libparinfer_rust.so
            ;;
        * )
            ;;
    esac

    if [ -z "$_libname" ]; then
        printe_h2 "could not determine libname, skipping parinfer-rust..."
        return 1
    fi

    if ! forced && [ -f "$_verdir/$_libname" ]; then
        printe_info "$_verdir/$_libname already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive parinfer-rust.tar.gz eraserhd/parinfer-rust "v$PARINFER_RUST_VERSION"
    verify_shasum parinfer-rust.tar.gz "$PARINFER_RUST_SHA256"
    run_tar -C "$BUILD_DIR" -xzf parinfer-rust.tar.gz
    rm parinfer-rust.tar.gz

    cd "$BUILD_DIR/parinfer-rust-${PARINFER_RUST_VERSION}" || exit 1
    cargo build --release
    install -d "$_verdir"
    install -m0755 "target/release/$_buildname" "$_verdir/$_libname"
    make_link "$_verdir/$_libname" "$_installdir/$_libname"
}

run_with_flavors "$@"
