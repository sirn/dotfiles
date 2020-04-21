#!/bin/sh -e
#
# Install Cloudflared.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

CFD_VER=2020.2.1
CFD_SHA256=288a9c1fd352fb32c2d74194ab90b0502748c6d51c68421803b834cccf731501

_preflight() {
    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return 1
    fi
}

_run_dev() {
    _install_cloudflared
}

_install_cloudflared() {
    printe_h2 "Installing cloudflared..."

    if ! forced && [ -f "$HOME/.local/bin/cloudflared" ]; then
        printe_info "$HOME/.local/bin/cloudflared already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive cloudflared.tar.gz cloudflare/cloudflared $CFD_VER
    verify_shasum cloudflared.tar.gz $CFD_SHA256
    run_tar -C "$BUILD_DIR" -xzf cloudflared.tar.gz
    rm cloudflared.tar.gz

    worksrc="$BUILD_DIR/go/src/github.com/cloudflare"
    mkdir -p "$worksrc"
    mv \
        "$BUILD_DIR/cloudflared-$CFD_VER" \
        "$worksrc/cloudflared"

    cd "$worksrc/cloudflared/cmd/cloudflared" || exit 1
    mkdir -p "$HOME/.local/bin"
    go install .

    cd "$BUILD_DIR/go/bin" || exit 1
    install -m0755 "cloudflared" "$HOME/.local/bin/cloudflared"
    printe_info "cloudflared successfully installed"
}

run_with_flavors "$@"
