#!/bin/sh -e
#
# Install Kubernetes CLI.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

# Using 1.16 since we required an updated vendor:
# See https://github.com/kubernetes/kubernetes/tree/master/vendor
KUBECTL_VER=1.16.0-alpha.0
KUBECTL_SHA256=cc8c1214d311d9b78b3e6376d4cff6ad6c98df10df31ad887464623ec0684b53

GOPATH="$BUILD_DIR/go"
PATH="$GOPATH/bin:$PATH"
export GOPATH

_run() {
    printe_h2 "Installing kubectl..."

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    if is_force || file_absent "$HOME/.local/bin/kubectl"; then
        cd "$BUILD_DIR" || exit 1

        fetch_gh_archive kubectl.tar.gz kubernetes/kubernetes "v$KUBECTL_VER"
        verify_shasum kubectl.tar.gz $KUBECTL_SHA256
        tar -C "$BUILD_DIR" -xzf kubectl.tar.gz
        rm kubectl.tar.gz

        mkdir -p "$BUILD_DIR/go/src/k8s.io"
        mv \
            "$BUILD_DIR/kubernetes-$KUBECTL_VER" \
            "$BUILD_DIR/go/src/k8s.io/kubernetes"

        cd "$BUILD_DIR/go/src/k8s.io/kubernetes/cmd/kubectl" || exit 1
        go install .
        cp "$BUILD_DIR/go/bin/kubectl" "$HOME/.local/bin/kubectl"
        printe_info "kubectl has been successfully installed"
    fi
}

_run
