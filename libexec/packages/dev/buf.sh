#!/bin/sh -e
#
# Install buf packages.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

BUF_VERSION=1.9.0

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping buf..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing buf..."
    _install_buf
}

_install_buf() {
    asdf_plugin buf https://github.com/truepay/asdf-buf
    asdf_install buf "$BUF_VERSION" global
}

run_with_flavors "$@"
