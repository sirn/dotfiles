#!/bin/sh -e
#
# Install Cloudflared.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

CFD_VER=2020.2.1
CFD_SHA256=288a9c1fd352fb32c2d74194ab90b0502748c6d51c68421803b834cccf731501

_run() {
    printe_h2 "Installing cloudflared..."

    if ! forced && [ -f "$HOME/.local/bin/cloudflared" ]; then
        printe_info "$HOME/.local/bin/cloudflared already exists, skipping..."
        return
    fi

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive cloudflared.tar.gz cloudflare/cloudflared $CFD_VER
    verify_shasum cloudflared.tar.gz $CFD_SHA256
    tar -C "$BUILD_DIR" -xzf cloudflared.tar.gz
    rm cloudflared.tar.gz

    worksrc="$BUILD_DIR/go/src/github.com/cloudflare"
    mkdir -p "$worksrc"
    mv \
        "$BUILD_DIR/cloudflared-$CFD_VER" \
        "$worksrc/cloudflared"

    cd "$worksrc/cloudflared/cmd/cloudflared" || exit 1
    mkdir -p "$HOME/.local/bin"
    go install .
    cp "$BUILD_DIR/go/bin/cloudflared" "$HOME/.local/bin/cloudflared"
    printe_info "cloudflared successfully installed"
}

_run
