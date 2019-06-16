#!/bin/sh -e
#
# Install Kubernetes Helm.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

BUILD_DIR=$(make_temp)

HELM_VER=2.13.1
HELM_SHA256=328f355050ebaecf6420aba85e44c1e86c1aa429991ec9102cc2b28ba9bc5536


## Environment variables
##

GOPATH="$BUILD_DIR/go"; export GOPATH
PATH="$GOPATH/bin:$PATH"


## Run
##

_run() {
    printe_h2 "Installing helm..."

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    if is_force || file_absent "$HOME/.local/bin/helm"; then
        cd "$BUILD_DIR" || exit 1

        case $(uname) in
            FreeBSD | OpenBSD )
                require_bin gmake
                ;;
        esac

        fetch_gh_archive helm.tar.gz helm/helm "v$HELM_VER"
        verify_shasum helm.tar.gz $HELM_SHA256
        tar -C "$BUILD_DIR" -xzf helm.tar.gz
        rm helm.tar.gz

        mkdir -p "$BUILD_DIR/go/src/k8s.io"
        mv "$BUILD_DIR/helm-$HELM_VER" "$BUILD_DIR/go/src/k8s.io/helm"

        cd "$BUILD_DIR/go/src/k8s.io/helm"
        gmake bootstrap build

        cp "$BUILD_DIR/go/src/k8s.io/helm/bin/helm" "$HOME/.local/bin/helm"
        cp "$BUILD_DIR/go/src/k8s.io/helm/bin/tiller" "$HOME/.local/bin/tiller"
        printe "kubectl has been successfully installed"
    fi
}

_run
