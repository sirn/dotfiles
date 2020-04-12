#!/bin/sh -e
#
# Install nim packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../../share/bootstrap/utils.sh"

_preflight() {
    PATH=$HOME/.nimble/bin:$PATH

    _setup_choosenim
    _setup_nim

    if ! command -v nimble >/dev/null; then
        printe_info "nimble is not installed, skipping nim packages..."
        return 1
    fi
}

_setup_choosenim() {
    # Skip without force flag if we already have system-wide choosenim
    # e.g. for musl system where there's no prebuilt choosenim-musl binary
    # and we must install choosenim from distro package.
    if command -v choosenim >/dev/null; then
        return
    fi

    PATH=$HOME/.nimble/bin:$PATH

    if ! forced && command -v choosenim >/dev/null; then
        printe_info "choosenim already installed, skipping..."
        return
    fi

    printe_h2 "Installing choosenim..."
    fetch_url - https://nim-lang.org/choosenim/init.sh | sh -s - -y
}

_setup_nim() {
    PATH=$HOME/.nimble/bin:$PATH

    if ! forced && command -v nim >/dev/null; then
        printe_info "nim already installed, skipping..."
        return
    fi

    printe_h2 "Installing nim..."
    choosenim -y stable
}

_nimble_install() {
    bin=$1; shift
    pkg=$1; shift

    PATH=$HOME/.nimble/bin:$PATH
    pkgbin_path=$HOME/.nimble/bin/$bin

    if ! forced && [ -f "$pkgbin_path" ]; then
        printe_info "$pkgbin_path already exists, skipping..."
        return
    fi

    nimble install "$pkg"
}

run_with_flavors "$@"
