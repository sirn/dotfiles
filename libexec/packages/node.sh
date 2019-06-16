#!/bin/sh -e
#
# Install node packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

_run() {
    if ! command -v npm >/dev/null; then
       printe_h2 "Installing node packages..."
       printe_info "npm is not installed, skipping..."
       return 1
    fi

    npm set prefix="$HOME/.local"
}

_run_dev() {
    npm install -g \
        bower \
        eslint \
        prettier \
        solc \
        solium \
        stylelint \
        stylelint-config-recommended \
        stylelint-config-recommended-scss \
        stylelint-scss \
        tern \
        tslint \
        typescript \
        wscat \
        yarn
}

run_with_flavors "$@"
