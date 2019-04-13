#!/bin/sh -e
#
# Install kapitan.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


## Preparation
## Kapitan has a strict version lock and require updating other dependencies
## every time Kapitan is updated. We're locking the version here to ensure
## custom-built dependencies (e.g. py-cryptography) works.
##

kapitan_ver=0.23.0
cryptography_ver=2.6.1
jsonnet_ver=0.12.1

printe_h2 "Installing kapitan..."


## Setup py-cryptography
## py-cryptography doesn't work well under LibreSSL (e.g. OpenBSD) without
## patching the CFFI source to disable some OpenSSL features.
##

if ! "$HOME/.asdf/shims/python3" -c 'import cryptography' >/dev/null 2>&1; then
    case $(openssl version | tr '[:upper:]' '[:lower:]') in
        libressl* )
            printe_info "Patching py-cryptography for libressl..."

            fetch_gh_archive - "pyca/cryptography" "$cryptography_ver" | tar -C "$build_dir" -xzf -
            cd "$build_dir/cryptography-$cryptography_ver" || exit 1

            for filename in \
                security/py-cryptography/patches/patch-src__cffi_src_openssl_ssl_py \
                    security/py-cryptography/patches/patch-src__cffi_src_openssl_x509_vfy_py; do
                fetch_gh_raw - \
                             openbsd/ports \
                             6fb634bd079c63a17a8f3f0a00a4e119b91bc9ad \
                             $filename |
                    patch -p0
            done

            "$HOME/.asdf/shims/pip3" install .
            ;;

        * )
            ;;
    esac
fi


## Setup py-jsonnet
## We need to patch setup.py to explicitly call gmake instead of make
## because py-jsonnet setup.py assumes make is gmake. py-jsonnet also
## assumes od is GNU-compatible.
##

if ! "$HOME/.asdf/shims/python3" -c 'import _jsonnet' >/dev/null 2>&1; then
    case $platform in
        freebsd | openbsd )
            printe_info "Patching py-jsonnet for $(uname)..."
            require_gmake "py-jsonnet"

            fetch_gh_archive - google/jsonnet "v$jsonnet_ver" | tar -C "$build_dir" -xzf -
            cd "$build_dir/jsonnet-$jsonnet_ver" || exit 1
            sed "s/'make'/'gmake'/g" < setup.py > setup.py.new
            mv setup.py.new setup.py

            od_bin="od"
            if [ "$platform" = "openbsd" ]; then
                require_coreutils "jsonnet"
                od_bin=ggod
            fi

            env \
                OD=$od_bin \
                CC=cc \
                CXX=c++ \
                CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
                "$HOME/.asdf/shims/pip3" install .
            ;;

        * )
            env \
                CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
                "$HOME/.asdf/shims/pip3" install jsonnet==$jsonnet_ver
            ;;
    esac
fi


## Setup kapitan
##

env "$HOME/.asdf/shims/pip3" install kapitan==$kapitan_ver
"$HOME/.asdf/bin/asdf" reshim python
