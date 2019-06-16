#!/bin/sh -e
#
# Install language runtime and its packages with asdf version manager.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

# shellcheck source=../../share/bootstrap/asdf.sh
. "$BASE_DIR/share/bootstrap/asdf.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

_run() {
    printe_h2 "Installing asdf..."
    git_clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" v0.7.2
}

_run_dev() {
    _asdf_plugin elixir https://github.com/asdf-vm/asdf-elixir.git 780a865
    _asdf_plugin erlang https://github.com/asdf-vm/asdf-erlang.git 3934206
    _asdf_plugin python https://github.com/tuvistavie/asdf-python.git cec7aa0
    _asdf_plugin ruby https://github.com/asdf-vm/asdf-ruby.git de1fb60

    _asdf_install elixir 1.8.1 global
    _asdf_install erlang 21.3.3 global
    _asdf_install python 3.7.3 global
    _asdf_install ruby 2.6.2 global

    _asdf_pkg python pip3 install -U \
        black \
        ipwhois \
        pip \
        pipenv \
        poetry \
        pre-commit \
        proselint \
        pylint \
        sphinx \
        virtualenv

    _asdf_pkg ruby gem install --no-document sqlint
}

run_with_flavors "$@"
