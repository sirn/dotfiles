#!/bin/sh -e
#
# Install python packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

PYTHON_VERSION=3.9.1
PYTHON_VERSION_PATH=$ASDF_DIR/installs/python/$PYTHON_VERSION

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping python..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing python..."
    _install_python
}

_install_python() {
    _asdf_plugin python https://github.com/danhper/asdf-python
    _asdf_install python "$PYTHON_VERSION" global
}

_run_dev() {
    _pip3_install poetry
    _pip3_install black
    _pip3_install flake8
    _pip3_install pyls python-language-server
    _pip3_install pyls_black pyls-black
    _pip3_install pyls_isort pyls-isort
    _pip3_install pyls_mypy pyls-mypy
    _asdf_reshim python
}

_pip3_install() {
    dir=$1; shift
    pkg=$1

    if [ $# -gt 0 ]; then
        shift
    fi

    if [ -z "$pkg" ]; then
        pkg=$dir
    fi

    python_major=${PYTHON_VERSION%%.${PYTHON_VERSION##*.}}
    pkgdir_path=$PYTHON_VERSION_PATH/lib/python$python_major/site-packages/$dir

    if ! forced && [ -d "$pkgdir_path" ]; then
        printe_info "$pkgdir_path already exists, skipping..."
        return
    fi

    pip3 install "$pkg" "$@"
}

run_with_flavors "$@"
