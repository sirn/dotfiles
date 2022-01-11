#!/bin/sh -e
#
# Install python packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

PYTHON_VERSION=3.10.1
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
    asdf_plugin python https://github.com/danhper/asdf-python

    if command -v port >/dev/null && ! command -v brew >/dev/null; then
        PATH="$BASE_DIR/libexec/mpshims:$PATH"
        export PATH
    fi

    asdf_install python "$PYTHON_VERSION" global
}

_run_dev() {
    PATH=$ASDF_DIR/shims:$PATH; export PATH

    if [ -d /opt/local ]; then
        CFLAGS="-I/opt/local/include"; export CFLAGS
        CPPFLAGS="-I/opt/local/include"; export CPPFLAGS
        LDFLAGS="-L/opt/local/lib"; export LDFLAGS
    fi

    _pip3_install poetry
    _pip3_install black
    _pip3_install flake8
    _pip3_install pylsp python-lsp-server
    _pip3_install mypy_ls mypy-ls
    _pip3_install pyls_flake8 pyls-flake8
    _pip3_install pyls_isort pyls-isort
    _pip3_install pylsp_black python-lsp-black

    _pip3_install ansible
    _pip3_install ansible-lint
    _pip3_install proselint
    _pip3_install tmuxp
    _pip3_install yamllint

    asdf_reshim python
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

    asdf_exec pip3 install "$pkg" "$@"
}

run_with_flavors "$@"
