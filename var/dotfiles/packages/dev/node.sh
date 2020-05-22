#!/bin/sh -e
#
# Install node packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

_preflight() {
    if ! command -v npm >/dev/null; then
       printe_h2 "npm is not installed, skipping node packages..."
       return 1
    fi
}

_run() {
    npm set prefix="$HOME/.local"
}

_run_dev() {
    printe_h2 "Installing npm dev packages..."

    npm install -g --force \
        eslint \
        npm \
        npm-upgrade \
        npx \
        prettier \
        stylelint \
        stylelint-config-recommended \
        stylelint-config-recommended-scss \
        stylelint-scss \
        tern \
        typescript \
        typescript-language-server \
        yaml-language-server
}

run_with_flavors "$@"
