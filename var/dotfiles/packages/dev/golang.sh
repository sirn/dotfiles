#!/bin/sh -e
#
# Install Golang packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

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
}

_go_get() {
    GOPATH=$HOME/Dev/go/gopath
    bin=$1; shift

    if ! forced && [ -f "$GOPATH/bin/$bin" ]; then
        printe_info "$GOPATH/bin/$bin already exists, skipping..."
        return
    fi

    go get -v -u "$@"
}

run_with_flavors "$@"
