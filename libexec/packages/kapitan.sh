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

JSONNET_VER=0.12.1
JSONNET_SHA256=257c6de988f746cc90486d9d0fbd49826832b7a2f0dbdb60a515cc8a2596c950

_setup_jsonnet() {
    if ! forced && $PIP show jsonnet==$JSONNET_VER >/dev/null 2>&1; then
        printe_info "py-jsonnet already installed"
        return
    fi

    case $PLATFORM in
        freebsd )
            printe_info "Patching py-jsonnet for $PLATFORM..."

            if ! command -v gmake >/dev/null; then
                printe_info "gmake is required to be installed, skipping.."
                return
            fi

            if ! command -v od >/dev/null; then
                printe_info "od is required to be installed, skipping..."
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
                OD=od \
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
