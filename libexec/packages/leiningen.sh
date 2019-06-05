#!/bin/sh -e
#
# Install leiningen.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*

LEIN_VER=2.9.1
LEIN_SHA256=32acacc8354627724d27231bed8fa190d7df0356972e2fd44ca144c084ad4fc7


## Run
##

_run_dev() {
    printe_h2 "Installing leiningen..."

    if is_force || file_absent "$HOME/.local/bin/lein"; then
        fetch_gh_raw "$HOME/.local/bin/lein.new" technomancy/leiningen $LEIN_VER bin/lein
        verify_shasum "$HOME/.local/bin/lein.new" $LEIN_SHA256

        mv "$HOME/.local/bin/lein.new" "$HOME/.local/bin/lein"
        chmod 0755 "$HOME/.local/bin/lein"
        printe "leiningen has been successfully installed"
    fi
}

run_with_flavors "$FLAVORS"
