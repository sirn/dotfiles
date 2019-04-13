#!/bin/sh -e
#
# Install leiningen.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Setup
##

printe_h2 "Installing leiningen..."

if file_absent "$HOME/.local/bin/lein"; then
    fetch_gh_raw "$HOME/.local/bin/lein" technomancy/leiningen stable bin/lein
    chmod 0755 "$HOME/.local/bin/lein"
    printe "leiningen has been successfully installed"
fi
