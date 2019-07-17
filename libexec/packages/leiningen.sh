#!/bin/sh -e
#
# Install leiningen.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

LEIN_VER=2.9.1
LEIN_SHA256=32acacc8354627724d27231bed8fa190d7df0356972e2fd44ca144c084ad4fc7

_run() {
    printe_h2 "Installing leiningen..."

    if ! forced && [ -f "$HOME/.local/bin/lein" ]; then
        printe_info "$HOME/.local/bin/lein already exists, skipping..."
        return
    fi

    fetch_gh_raw \
        "$HOME/.local/bin/lein.new" \
        technomancy/leiningen \
        $LEIN_VER \
        bin/lein

    verify_shasum "$HOME/.local/bin/lein.new" $LEIN_SHA256

    mv "$HOME/.local/bin/lein.new" "$HOME/.local/bin/lein"
    chmod 0755 "$HOME/.local/bin/lein"
    printe_info "leiningen has been successfully installed"
}

_run
