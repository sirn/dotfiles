#!/bin/sh -e
#
# Install leiningen.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Setup
##

printe_h2 "Installing leiningen..."

if file_absent "$HOME/.local/bin/lein"; then
    fetch_gh_raw "$HOME/.local/bin/lein" technomancy/leiningen stable bin/lein
    chmod 0755 "$HOME/.local/bin/lein"
    printe "leiningen has been successfully installed"
fi
