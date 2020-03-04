#!/bin/sh -e
#
# Install Google Cloud SDK
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

CLOUDSDK_VER=280.0.0

case $(get_platform) in
    ubuntu | debian | void )
        CLOUDSDK_DIST=google-cloud-sdk-${CLOUDSDK_VER}-linux-x86_64.tar.gz
        CLOUDSDK_SHA256=11950f1db216ec7dc3abaf80722fb80518c38e279bd76b6924326fe660c209cf
        ;;

    darwin )
        CLOUDSDK_DIST=google-cloud-sdk-${CLOUDSDK_VER}-darwin-x86_64.tar.gz
        CLOUDSDK_SHA256=c9554507bc217a503b42bef7dfa72179bae57ad7e4e696af4205c50b373d3576
        ;;

    * )
        ;;
esac

CLOUDSDK_URL=https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/$CLOUDSDK_DIST

_run() {
    printe_h2 "Installing google-cloud-sdk..."

    if ! forced && [ -f "$HOME/.local/lib/google-cloud-sdk/bin/gcloud" ]; then
        printe_info "$HOME/.local/lib/google-cloud-sdk already exists, skipping..."
        return
    fi

    if [ -z "$CLOUDSDK_DIST" ]; then
        printe_info "Unsupported platform, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_url gcloud-sdk.tar.gz "$CLOUDSDK_URL"
    verify_shasum gcloud-sdk.tar.gz "$CLOUDSDK_SHA256"
    tar -C "$BUILD_DIR" -xzf gcloud-sdk.tar.gz
    rm gcloud-sdk.tar.gz

    mkdir -p "$HOME/.local/lib"
    mv "$BUILD_DIR/google-cloud-sdk" "$HOME/.local/lib"
    printe_info "google-cloud-sdk successfully installed"
}

_run
