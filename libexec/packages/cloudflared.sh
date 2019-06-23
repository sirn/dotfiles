#!/bin/sh -e
#
# Install Cloudflared.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

CFD_VER=2019.6.0
CFD_SHA256=33df5722de9c43640fb287b62b619299ec89881cd1d193436636ac4fe2a097f4

GOPATH="$BUILD_DIR/go"
PATH="$GOPATH/bin:$PATH"
export GOPATH

_run() {
    printe_h2 "Installing cloudflared.."

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    if is_force || file_absent "$HOME/.local/bin/cloudflared"; then
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
        go install .
        cp "$BUILD_DIR/go/bin/cloudflared" "$HOME/.local/bin/cloudflared"
        printe_info "cloudflared has been successfully installed"
    fi
}

_run
