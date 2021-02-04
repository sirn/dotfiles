#!/bin/sh -e
#
# Install nodejs packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

NODEJS_VERSION=ref:v10.23.2
NODEJS_VERSION_PATH=$ASDF_DIR/installs/nodejs/ref-v10.23.2

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping nodejs..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing nodejs..."
    _install_nodejs
}

_install_nodejs() {
    _asdf_plugin nodejs https://github.com/asdf-vm/asdf-nodejs

    if [ ! -f "$HOME/.gnupg/asdf-nodejs.gpg" ]; then
        sh -c "${ASDF_DATA_DIR:=$HOME/.asdf}"/plugins/nodejs/bin/import-release-team-keyring
    fi

    NODEJS_CHECK_SIGNATURES=no; export NODEJS_CHECK_SIGNATURES
    _asdf_install nodejs "$NODEJS_VERSION" global
}

_run_dev() {
    _npm_install eslint
    _npm_install npm-upgrade
    _npm_install prettier
    _npm_install stylelint
    _npm_install stylelint-config-recommended
    _npm_install stylelint-scss
    _npm_install tern
    _npm_install typescript
    _npm_install typescript-language-server
    _asdf_reshim nodejs
}

_npm_install() {
    dir=$1; shift
    pkg=$1

    if [ $# -gt 0 ]; then
        shift
    fi

    if [ -z "$pkg" ]; then
        pkg=$dir
    fi

    pkgdir_path=$NODEJS_VERSION_PATH/.npm/lib/node_modules/$pkg

    if ! forced && [ -d "$pkgdir_path" ]; then
        printe_info "$pkgdir_path already exists, skipping..."
        return
    fi

    npm install -g "$pkg" "$@"
}

run_with_flavors "$@"
