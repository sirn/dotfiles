#!/bin/sh -e
#
# Install Google Cloud SDK
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

CLOUDSDK_VER=280.0.0
HELM_VER=2.16.6

case $(get_platform) in
    ubuntu | debian | void )
        CLOUDSDK_DIST=google-cloud-sdk-${CLOUDSDK_VER}-linux-x86_64.tar.gz
        CLOUDSDK_SHA256=11950f1db216ec7dc3abaf80722fb80518c38e279bd76b6924326fe660c209cf

        HELM_DIST=helm-v${HELM_VER}-linux-amd64.tar.gz
        HELM_SHA256=e38fea59bc382feb0f80322d582266465d76ab72acdc0079c2350cc3fd8a3f4c
        ;;

    darwin )
        CLOUDSDK_DIST=google-cloud-sdk-${CLOUDSDK_VER}-darwin-x86_64.tar.gz
        CLOUDSDK_SHA256=c9554507bc217a503b42bef7dfa72179bae57ad7e4e696af4205c50b373d3576

        HELM_DIST=helm-v${HELM_VER}-darwin-amd64.tar.gz
        HELM_SHA256=7cccf13b3e821dda1e0ffa56ca6bec2648f2dae6fb8c12f22a9be2396698771f
        ;;

    * )
        ;;
esac

CLOUDSDK_URL=https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/$CLOUDSDK_DIST
HELM_URL=https://get.helm.sh/$HELM_DIST

KUBECTX_VER=0.8.0
KUBECTX_SHA256=7acbb574f2b9cb82c03b2ceaf1d5cf312eddf1cefa12ecf6bc6bf0478511f809

KUSTOMIZE_VER=3.5.4
KUSTOMIZE_SHA256=71c49d7a72c5e996c0388e8f7d2699a9b00a035960aa81b4b25c1c24c03593e2

_preflight() {
    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return 1
    fi
}

_run_dev() {
    if [ -n "$CLOUDSDK_DIST" ]; then
        _install_cloudsdk
        _install_kubectl
    fi

    if [ -n "$HELM_DIST" ]; then
        _install_helm
    fi

    _install_kustomize
    _install_kubectx
}

_install_cloudsdk() {
    printe_h2 "Installing google-cloud-sdk..."

    if ! forced && [ -f "$HOME/.local/lib/google-cloud-sdk/bin/gcloud" ]; then
        printe_info "$HOME/.local/lib/google-cloud-sdk already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_url gcloud-sdk.tar.gz "$CLOUDSDK_URL"
    verify_shasum gcloud-sdk.tar.gz "$CLOUDSDK_SHA256"
    run_tar -C "$BUILD_DIR" -xzf gcloud-sdk.tar.gz
    rm gcloud-sdk.tar.gz

    mkdir -p "$HOME/.local/lib"
    mv "$BUILD_DIR/google-cloud-sdk" "$HOME/.local/lib"
    printe_info "google-cloud-sdk successfully installed"
}

_install_kubectl() {
    printe_h2 "Installing kubectl..."

    if ! forced && [ -f "$HOME/.local/lib/google-cloud-sdk/bin/kubectl" ]; then
        printe_info "$HOME/.local/lib/google-cloud-sdk/bin/kubectl already exists, skipping..."
        return
    fi

    gcloud components install kubectl
    printe_info "kubectl successfully installed"
}

_install_helm() {
    printe_h2 "Installing helm..."

    if ! forced && [ -f "$HOME/.local/bin/helm" ]; then
        printe_info "$HOME/.local/bin/helm already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_url helm.tar.gz "$HELM_URL"
    verify_shasum helm.tar.gz "$HELM_SHA256"
    run_tar -C "$BUILD_DIR" -xzf helm.tar.gz
    rm helm.tar.gz

    mkdir -p "$HOME/.local/bin"
    find "$BUILD_DIR" \
         \( -iname helm -or -iname tiller \) -executable \
         -prune \
         -exec install -m0755 '{}' "$HOME/.local/bin" \;

    printe_info "helm successfully installed"
}

_install_kubectx() {
    printe_h2 "Installing kubectx..."

    if ! forced && [ -f "$HOME/.local/bin/kubectx" ]; then
        printe_info "$HOME/.local/bin/kubectx already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive kubectx.tar.gz ahmetb/kubectx v$KUBECTX_VER
    verify_shasum kubectx.tar.gz $KUBECTX_SHA256
    run_tar -C "$BUILD_DIR" -xzf kubectx.tar.gz
    rm kubectx.tar.gz

    cd "$BUILD_DIR/kubectx-$KUBECTX_VER" || exit 1
    install -m0755 "kubectx" "$HOME/.local/bin/kubectx"
    install -m0755 "kubens" "$HOME/.local/bin/kubens"
    printe_info "kubectx successfully installed"
}

_install_kustomize() {
    printe_h2 "Installing kustomize..."

    if ! forced && [ -f "$HOME/.local/bin/kustomize" ]; then
        printe_info "$HOME/.local/bin/kustomize already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive \
        kustomize.tar.gz \
        kubernetes-sigs/kustomize \
        kustomize/v$KUSTOMIZE_VER
    verify_shasum kustomize.tar.gz $KUSTOMIZE_SHA256
    run_tar -C "$BUILD_DIR" -xzf kustomize.tar.gz
    rm kustomize.tar.gz

    unset GO111MODULES
    worksrc=$BUILD_DIR/kustomize-kustomize-v$KUSTOMIZE_VER/kustomize

    cd "$worksrc" || exit 1
    go build .
    install -m0755 "$worksrc/kustomize" "$HOME/.local/bin/kustomize"
    printe_info "kustomize successfully installed"
}

run_with_flavors "$@"
