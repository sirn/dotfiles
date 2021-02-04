#!/bin/sh -e
#
# Install Cloudflared.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"

CFD_VER=2021.1.5
CFD_SHA256=553c416d36a7ac66a5047f377749fdfd8278728b5afb8df89a2b09c91f73a6cb

_preflight() {
    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return 1
    fi
}

_run() {
    _install_cloudflared
}

_install_cloudflared() {
    printe_h2 "Installing cloudflared..."

    _verdir=$HOME/.cache/local-pkg/cloudflared/$CFD_VER
    _binpath=$HOME/.local/bin/cloudflared

    if ! forced && [ -f "$_verdir/bin/cloudflared" ]; then
        printe_info "$_verdir/bin/cloudflared already exists, skipping..."
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
    date=$(date -u '+%Y-%m-%d-%H%M UTC')
    go install \
       -v \
       -ldflags="-X \"main.Version=$CFD_VER\" -X \"main.BuildTime=$date\"" \
       .

    cd "$BUILD_DIR/go/bin" || exit 1
    install -d "$_verdir/bin"
    install -m0755 "cloudflared" "$_verdir/bin/cloudflared"
    make_link "$_verdir/bin/cloudflared" "$_binpath"
}

run_with_flavors "$@"
