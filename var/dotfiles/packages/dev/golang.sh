#!/bin/sh -e
#
# Install Golang packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

GOLANGCI_LINT_VER=1.27.0

_preflight() {
    if ! command -v go >/dev/null; then
       printe_h2 "go is not installed, skipping golang packages..."
       return 1
    fi

    return 0
}

_run_dev() {
    printe_h2 "Installing golang dev packages..."

    _go_get gocode github.com/nsf/gocode
    _go_get goimports golang.org/x/tools/cmd/goimports
    _go_get golint golang.org/x/lint/golint
    _go_get gopls golang.org/x/tools/gopls

    _install_golangci_lint
}

_install_golangci_lint() {
    printe_h2 "Installing golangci-lint..."

    _bindir=$HOME/Dev/go/gopath/bin
    if ! forced && [ -f "$_bindir/golangci-lint" ]; then
        printe_info "$_bin/golangci-lint already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    _os=$(uname | tr '[:upper:]' '[:lower:]')

    fetch_gh_release \
        golangci-lint.tar.gz \
        golangci/golangci-lint \
        v$GOLANGCI_LINT_VER \
        golangci-lint-$GOLANGCI_LINT_VER-"$_os"-amd64.tar.gz

    run_tar -C "$BUILD_DIR" -xzf golangci-lint.tar.gz
    rm golangci-lint.tar.gz

    _wrksrc=$BUILD_DIR/golangci-lint-$GOLANGCI_LINT_VER-linux-amd64
    install -Dm0755 "$_wrksrc/golangci-lint" "$_bindir"
    printe_info "golangci-lint successfully installed"
}

_go_get() {
    _bindir=$HOME/Dev/go/gopath/bin
    _bin=$1; shift

    if ! forced && [ -f "$_bindir/$_bin" ]; then
        printe_info "$_bindir/$_bin already exists, skipping..."
        return
    fi

    (
        cd "$BUILD_DIR" || exit 1
        GOPATH="$BUILD_DIR/go"
        go get -v -u "$@"
    )
}

run_with_flavors "$@"
