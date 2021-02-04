#!/bin/sh -e
#
# Install Google Cloud SDK
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"

_preflight() {
    if ! command -v bash >/dev/null; then
        printe_info "bash is not installed, skipping..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing gcloud..."

    _install_gcloud
    _install_gcloud_kubectl
}

_install_gcloud() {
    _sdkdir="$HOME/.local/lib/google-cloud-sdk"
    _binpath="$_sdkdir/bin/gcloud"

    if ! forced && [ -f "$_binpath" ]; then
        printe_info "$_binpath already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1
    fetch_url install.sh https://sdk.cloud.google.com/
    bash install.sh --disable-prompts --install-dir="$(dirname "$_sdkdir")"
}

_install_gcloud_kubectl() {
    _sdkdir="$HOME/.local/lib/google-cloud-sdk"
    _binpath="$_sdkdir/bin/kubectl"

    if ! forced && [ -f "$_binpath" ]; then
        printe_info "$_binpath already exists, skipping..."
        return
    fi

    "$_sdkdir/bin/gcloud" components install -q kubectl
}

run_with_flavors "$@"
