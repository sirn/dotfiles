#!/bin/sh -e
#
# Install Kubernetes CLI.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*
BUILD_DIR=$(make_temp)

# Using 1.16 since we required an updated vendor:
# See https://github.com/kubernetes/kubernetes/tree/master/vendor
KUBECTL_VER=1.16.0-alpha.0
KUBECTL_SHA256=cc8c1214d311d9b78b3e6376d4cff6ad6c98df10df31ad887464623ec0684b53


## Environment variables
##

GOPATH="$BUILD_DIR/go"; export GOPATH
PATH="$GOPATH/bin:$PATH"


## Run
##

_run_kubernetes() {
    printe_h2 "Installing kubectl..."

    if is_force || file_absent "$HOME/.local/bin/kubectl"; then
        cd "$BUILD_DIR" || exit 1

        require_bin go

        fetch_gh_archive kubectl.tar.gz kubernetes/kubernetes "v$KUBECTL_VER"
        verify_shasum kubectl.tar.gz $KUBECTL_SHA256
        tar -C "$BUILD_DIR" -xzf kubectl.tar.gz
        rm kubectl.tar.gz

        mkdir -p "$BUILD_DIR/go/src/k8s.io"
        mv "$BUILD_DIR/kubernetes-$KUBECTL_VER" "$BUILD_DIR/go/src/k8s.io/kubernetes"

        cd "$BUILD_DIR/go/src/k8s.io/kubernetes/cmd/kubectl" || exit 1
        go install .
        cp "$BUILD_DIR/go/bin/kubectl" "$HOME/.local/bin/kubectl"
        printe "kubectl has been successfully installed"
    fi
}

run_with_flavors "$FLAVORS"
