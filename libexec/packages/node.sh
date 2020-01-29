#!/bin/sh -e
#
# Install node packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

_run() {
    if ! command -v npm >/dev/null; then
       printe_h2 "npm is not installed, skipping node packages..."
       return 1
    fi

    npm set prefix="$HOME/.local"
}

_run_dev() {
    printe_h2 "Installing npm dev packages..."

    npm install -g \
        eslint \
        javascript-typescript-langserver \
        npm-upgrade \
        npx \
        prettier \
        solc \
        solium \
        stylelint \
        stylelint-config-recommended \
        stylelint-config-recommended-scss \
        stylelint-scss \
        tern \
        typescript \
        wscat
}

run_with_flavors "$@"
