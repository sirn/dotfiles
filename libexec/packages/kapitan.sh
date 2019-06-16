#!/bin/sh -e
#
# Install kapitan.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

PYTHON3="$HOME/.asdf/shims/python3"
PIP3="$HOME/.asdf/shims/pip3"

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
    if is_force || ! "$PYTHON3" -c 'import cryptography' >/dev/null 2>&1; then
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

                $PIP3 install .
                ;;

            * )
                ;;
        esac
    fi
}

_setup_jsonnet() {
    if is_force || ! "$PYTHON3" -c 'import _jsonnet' >/dev/null 2>&1; then
        case $(uname) in
            FreeBSD | OpenBSD )
                # We need to patch setup.py to explicitly call gmake instead of
                # make because py-jsonnet setup.py assumes make is gmake.
                # py-jsonnet also assumes od is GNU-compatible.

                printe_info "Patching py-jsonnet for $(uname)..."
                cd "$BUILD_DIR" || exit 1

                require_bin gmake

                fetch_gh_archive jsonnet.tar.gz google/jsonnet "v$JSONNET_VER"
                verify_shasum jsonnet.tar.gz $JSONNET_SHA256
                tar -C "$BUILD_DIR" -xzf jsonnet.tar.gz
                rm jsonnet.tar.gz

                cd "$BUILD_DIR/jsonnet-$JSONNET_VER" || exit 1
                sed "s/'make'/'gmake'/g" < setup.py > setup.py.new
                mv setup.py.new setup.py

                od_bin="od"
                if [ "$(uname)" = "OpenBSD" ]; then
                    require_bin ggod "Try \`pkg_add coreutils\`"
                    od_bin=ggod
                fi

                env \
                    OD=$od_bin \
                    CC=cc \
                    CXX=c++ \
                    CXXFLAGS="-fPIC \
-Iinclude -Ithird_party/md5 -Ithird_party/json \
-std=c++11" \
                    "$PIP3" install .
                ;;

            * )
                env \
                    CXXFLAGS="-fPIC \
-Iinclude -Ithird_party/md5 -Ithird_party/json \
-std=c++11" \
                    "$PIP3" install jsonnet==$JSONNET_VER
                ;;
        esac
    fi
}

_run() {
    printe_h2 "Installing kapitan..."

    if [ ! -e "$PYTHON3" ]; then
        printe_info "$PYTHON3 does not exists, skipping..."
        return
    fi

    _setup_cryptography
    _setup_jsonnet

    $PIP3 install kapitan==$KAPITAN_VER
    "$HOME/.asdf/bin/asdf" reshim python
}

_run
