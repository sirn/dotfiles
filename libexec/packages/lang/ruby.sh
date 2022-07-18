#!/bin/sh -e
#
# Install ruby packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

RUBY_VERSION=3.1.2

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping ruby..."
        return 1
    fi
}

_run() {
    _install_ruby
}

_install_ruby() {
    # https://github.com/openssl/openssl/issues/18720
    export CFLAGS=-Wno-error=implicit-function-declaration
    asdf_plugin ruby https://github.com/asdf-vm/asdf-ruby
    asdf_install ruby "$RUBY_VERSION" global
}

run_with_flavors "$@"
