#!/bin/sh -e
#
# Install kapitan.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

PLATFORM=$(get_platform)

PIP=
KAPITAN_VER=0.23.0

PYCRYPT_VER=2.6.1
PYCRYPT_SHA256=e6b77dddc068dcbb13c193602d7a40dc0bb348ceb107b14be083e42afa24ab83
PYCRYPT_OPENBSD_PATCH_COMMIT=6fb634bd079c63a17a8f3f0a00a4e119b91bc9ad
PYCRYPT_OPENBSD_PATCHES="\
security/py-cryptography/patches/patch-src__cffi_src_openssl_ssl_py
security/py-cryptography/patches/patch-src__cffi_src_openssl_x509_vfy_py
"

JSONNET_VER=0.12.1
JSONNET_SHA256=257c6de988f746cc90486d9d0fbd49826832b7a2f0dbdb60a515cc8a2596c950

_setup_cryptography() {
    if ! forced && $PIP show cryptography==$PYCRYPT_VER >/dev/null 2>&1; then
        printe_info "py-cryptography already installed"
        return
    fi

    case $(openssl version | tr '[:upper:]' '[:lower:]') in
        libressl* )
            # py-cryptography doesn't build under LibreSSL (e.g. OpenBSD)
            # without patching the CFFI source to disable some OpenSSL
            # features.

            printe_info "Patching py-cryptography for libressl..."
            cd "$BUILD_DIR" || exit 1

            fetch_gh_archive \
                cryptography.tar.gz \
                pyca/cryptography \
                "$PYCRYPT_VER"

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

            $PIP install --user .
            ;;

        * )
            ;;
    esac
}

_setup_jsonnet() {
    if ! forced && $PIP show jsonnet==$JSONNET_VER >/dev/null 2>&1; then
        printe_info "py-jsonnet already installed"
        return
    fi

    case $PLATFORM in
        freebsd | openbsd )
            printe_info "Patching py-jsonnet for $PLATFORM..."

            if ! command -v gmake >/dev/null; then
                printe_info "gmake is required to be installed, skipping.."
                return
            fi

            # py-jsonnet also assumes od is GNU-compatible.
            od_bin="od"
            if [ "$PLATFORM" = "openbsd" ]; then
                od_bin="ggod"
            fi

            if ! command -v $od_bin >/dev/null; then
                printe_info "$od_bin is required to be installed, skipping..."
                return
            fi

            cd "$BUILD_DIR" || exit 1

            fetch_gh_archive jsonnet.tar.gz google/jsonnet "v$JSONNET_VER"
            verify_shasum jsonnet.tar.gz $JSONNET_SHA256
            tar -C "$BUILD_DIR" -xzf jsonnet.tar.gz
            rm jsonnet.tar.gz

            # We need to patch setup.py to explicitly call gmake instead of
            # make because py-jsonnet setup.py assumes make is gmake.
            cd "$BUILD_DIR/jsonnet-$JSONNET_VER" || exit 1
            sed "s/'make'/'gmake'/g" < setup.py > setup.py.new
            mv setup.py.new setup.py

            env \
                OD=$od_bin \
                CC=cc \
                CXX=c++ \
                CXXFLAGS="-fPIC \
-Iinclude -Ithird_party/md5 -Ithird_party/json \
-std=c++11" \
                $PIP install --user .
            ;;

        * )
            env \
                CXXFLAGS="-fPIC \
-Iinclude -Ithird_party/md5 -Ithird_party/json \
-std=c++11" \
                $PIP install --user jsonnet==$JSONNET_VER
            ;;
    esac
}

_run() {
    printe_h2 "Installing kapitan..."

    PIP=$(detect_pip3)
    if [ -z "$PIP" ]; then
        printe_info "pip3 is not installed, skipping..."
        return 1
    fi

    _setup_cryptography
    _setup_jsonnet

    $PIP install --user kapitan==$KAPITAN_VER
}

_run
