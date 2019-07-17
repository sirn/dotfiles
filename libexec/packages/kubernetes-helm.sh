#!/bin/sh -e
#
# Install Kubernetes Helm.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

PLATFORM=$(get_platform)

HELM_VER=2.13.1
HELM_SHA256=328f355050ebaecf6420aba85e44c1e86c1aa429991ec9102cc2b28ba9bc5536

_run() {
    printe_h2 "Installing helm..."

    if ! forced && [ -f "$HOME/.local/bin/helm" ]; then
        printe_info "$HOME/.local/bin/helm already exists, skipping..."
        return
    fi

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    case $PLATFORM in
        freebsd | openbsd )
            if ! command -v gmake >/dev/null; then
                printe_info "gmake is required to be installed, skipping.."
                return
            fi
            ;;
    esac

    cd "$BUILD_DIR" || exit 1

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
}

_run
