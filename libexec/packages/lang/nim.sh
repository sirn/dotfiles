#!/bin/sh -e
#
# Install nim packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

NIM_VERSION=1.4.6
NIM_VERSION_PATH=$ASDF_DIR/installs/nim/$NIM_VERSION
NIM_VERSION_SRC=$HOME/.local/src/nim/$NIM_VERSION

NIMLSP_VERSION=0.2.6
NIMLSP_SHA256=664c9005a34e8e015bd7f4b1ff9d67e3a8a8f4f5a1998b355ba134e3700934fc

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping nim..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing nim..."
    _install_nim
    _install_nim_src

    printe_h2 "Installing nim packages..."
    _nimble_update
    _nimble_install nimr nimr
    _nimble_install nimcr nimcr
}

_install_nim() {
    asdf_plugin nim https://github.com/asdf-community/asdf-nim main
    asdf_install nim "$NIM_VERSION" global
}

_install_nim_src() {
    mkdir -p "$(dirname "$NIM_VERSION_SRC")"
    git_clone https://github.com/nim-lang/Nim "$NIM_VERSION_SRC" "v$NIM_VERSION"
}

_run_dev() {
    printe_h2 "Installing nim dev packages..."

    _install_nimlsp
}

_install_nimlsp() {
    pkgbin_path=$NIM_VERSION_PATH/nimble/bin/nimlsp
    if ! forced && [ -f "$pkgbin_path" ]; then
        printe_info "$pkgbin_path already exists, skipping..."
        return
    fi

    printe_h2 "Installing nimlsp..."

    cd "$BUILD_DIR" || exit 1
    fetch_gh_archive nimlsp.tar.gz PMunch/nimlsp v$NIMLSP_VERSION
    verify_shasum nimlsp.tar.gz $NIMLSP_SHA256
    run_tar -C "$BUILD_DIR" -xzf nimlsp.tar.gz
    rm nimlsp.tar.gz

    cd "$BUILD_DIR/nimlsp-$NIMLSP_VERSION" || exit 1
    nimble build -d:explicitSourcePath="$NIM_VERSION_SRC"
    mv nimlsp "$pkgbin_path"
    asdf_reshim nim
}

_nimble_install() {
    bin=$1; shift
    pkg=$1

    if [ $# -gt 0 ]; then
        shift
    fi

    if [ -z "$pkg" ]; then
        pkg=$bin
    fi

    pkgbin_path=$NIM_VERSION_PATH/nimble/bin/$bin

    if ! forced && [ -f "$pkgbin_path" ]; then
        printe_info "$pkgbin_path already exists, skipping..."
        return
    fi

    asdf_exec nimble install "$pkg" "$@"
}

_nimble_update() {
    index_path=$NIM_VERSION_PATH/nimble/packages_official.json
    _chk=$(find "$index_path" -mtime +259200 -print 2>/dev/null)

    if [ -n "$_chk" ] || [ ! -f "$index_path" ]; then
        printe_info "nimble package database is outdated; updating..."
        asdf_exec nimble update
    fi
}

run_with_flavors "$@"
