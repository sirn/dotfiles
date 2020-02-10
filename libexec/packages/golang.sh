#!/bin/sh -e
#
# Install Golang packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

_go_get() {
    GOPATH=$HOME/Dev/go/gopath
    bin=$1; shift

    if ! forced && [ -f "$GOPATH/bin/$bin" ]; then
        printe_info "$GOPATH/bin/$bin already exists, skipping..."
        return
    fi

    go get -v -u "$@"
}

_run() {
    if ! command -v go >/dev/null; then
       printe_h2 "go is not installed, skipping golang packages..."
       return 1
    fi
}

_run_dev() {
    printe_h2 "Installing golang dev packages..."

    _go_get gocode github.com/nsf/gocode
    _go_get goimports golang.org/x/tools/cmd/goimports
    _go_get golint golang.org/x/lint/golint
}

run_with_flavors "$@"
