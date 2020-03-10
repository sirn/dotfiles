#!/bin/sh -e
#
# Install kustomize.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

KUSTOMIZE_VER=3.5.4
KUSTOMIZE_SHA256=71c49d7a72c5e996c0388e8f7d2699a9b00a035960aa81b4b25c1c24c03593e2

_run() {
    printe_h2 "Installing kustomize..."

    if ! forced && [ -f "$HOME/.local/bin/kustomize" ]; then
        printe_info "$HOME/.local/bin/kustomize already exists, skipping..."
        return
    fi

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive \
        kustomize.tar.gz \
        kubernetes-sigs/kustomize \
        kustomize/v$KUSTOMIZE_VER
    verify_shasum kustomize.tar.gz $KUSTOMIZE_SHA256
    tar -C "$BUILD_DIR" -xzf kustomize.tar.gz
    rm kustomize.tar.gz

    unset GO111MODULES
    worksrc=$BUILD_DIR/kustomize-kustomize-v$KUSTOMIZE_VER/kustomize

    cd "$worksrc" || exit 1
    go build .
    cp "$worksrc/kustomize" "$HOME/.local/bin/kustomize"
    printe_info "kustomize successfully installed"
}

_run
