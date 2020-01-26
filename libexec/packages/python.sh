#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

PYTOOLS=${XDG_DATA_HOME:-$HOME/.local/share}/pytools
PIP=$PYTOOLS/bin/pip3

# py-cryptography doesn't build under LibreSSL (e.g. OpenBSD, Void)
# without patching the CFFI source to disable some OpenSSL
# features.

PYCRYPT_VER=2.8
PYCRYPT_SHA256=074e45d510156b3b45e0ffadf57a9655089922fdf84f611b239bb7732ccc18ee
PYCRYPT_OPENBSD_PATCH_COMMIT=e4337acb2f751d05d57fa4308df8065e886e025f
PYCRYPT_OPENBSD_PATCHES="\
security/py-cryptography/patches/patch-src__cffi_src_openssl_x509_vfy_py
"

_maybe_install_cryptography() {
    if ! forced && $PIP show cryptography==$PYCRYPT_VER >/dev/null 2>&1; then
        return
    fi

    case $(openssl version | tr '[:upper:]' '[:lower:]') in
        libressl* )
            :
            ;;

        * )
            return
            ;;
    esac

    printe_h2 "Patching py-cryptography for libressl..."
    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive cryptography.tar.gz pyca/cryptography $PYCRYPT_VER
    verify_shasum cryptography.tar.gz $PYCRYPT_SHA256
    tar -C "$BUILD_DIR" -xzf cryptography.tar.gz
    rm cryptography.tar.gz

    cd "$BUILD_DIR/cryptography-$PYCRYPT_VER" || exit 1

    for filename in $PYCRYPT_OPENBSD_PATCHES; do
        fetch_gh_raw - \
                     openbsd/ports \
                     "$PYCRYPT_OPENBSD_PATCH_COMMIT" \
                     "$filename" |
            patch -p0
    done

    $PIP install .
}

_run() {
    if ! command -v python3 >/dev/null; then
       printe_h2 "python3 is not installed, skipping python packages..."
       return 1
    fi

    if ! python3 -c 'import venv' 2>/dev/null; then
       printe_h2 "python3 venv is not available, skipping python packages..."
       return 1
    fi

    printe_h2 "Populating $PYTOOLS..."

    if [ -f "$PIP" ]; then
       printe_info "$PYTOOLS already exists, skipping..."
       return
    fi

    python3 -m venv --without-pip "$PYTOOLS"
    fetch_url /tmp/get-pip.py https://bootstrap.pypa.io/get-pip.py
    "$PYTOOLS/bin/python3" /tmp/get-pip.py
    rm /tmp/get-pip.py
}

_run_dev() {
    _maybe_install_cryptography

    printe_h2 "Installing python dev packages..."

    $PIP install \
         ansible \
         black \
         flake8 \
         ipwhois \
         pip \
         poetry \
         pre-commit \
         proselint \
         pyls-black \
         pyls-isort \
         pyls-mypy \
         python-language-server[flake8] \
         virtualenv
}

run_with_flavors "$@"
