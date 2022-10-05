#!/bin/sh -e
#
# Install doctl.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"

DOCTL_VER=1.78.0
DOCTL_SHA256=8b2fb24f00b98db8f6a730cf8d7703b2e0b8fd0abe23cd1a64e325d4e8f49ffe

_preflight() {
    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return 1
    fi
}

_run() {
    _install_doctl
}

_install_doctl() {
    _verdir=$HOME/.cache/local-pkg/doctl/$DOCTL_VER
    _binpath=$HOME/.local/bin/doctl

    if ! forced && [ -f "$_verdir/bin/doctl" ]; then
        printe_info "$_verdir/bin/doctl already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive doctl.tar.gz digitalocean/doctl v$DOCTL_VER
    verify_shasum doctl.tar.gz $DOCTL_SHA256
    run_tar -C "$BUILD_DIR" -xzf doctl.tar.gz
    rm doctl.tar.gz

    worksrc="$BUILD_DIR/go/src/github.com/digitalocean"
    mkdir -p "$worksrc"
    mv \
        "$BUILD_DIR/doctl-$DOCTL_VER" \
        "$worksrc/doctl"

    cd "$worksrc/doctl/cmd/doctl" || exit 1
    major=${DOCTL_VER%%.*}
    minor=${DOCTL_VER##"$major".}
    minor=${minor%%.*}
    patch=${DOCTL_VER##"$major"."$minor".}
    go install \
        -v \
        -ldflags=" \
          -X \"github.com/digitalocean/doctl.Major=$major\" \
          -X \"github.com/digitalocean/doctl.Minor=$minor\" \
          -X \"github.com/digitalocean/doctl.Patch=$patch\" \
       " \
        .

    cd "$BUILD_DIR/go/bin" || exit 1
    install -d "$_verdir/bin"
    install -m0755 "doctl" "$_verdir/bin/doctl"
    make_link "$_verdir/bin/doctl" "$_binpath"
}

run_with_flavors "$@"
