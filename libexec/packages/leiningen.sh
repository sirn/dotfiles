#!/bin/sh -e
#
# Install leiningen.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Setup
##

lein_ver=2.9.1
lein_sha256=32acacc8354627724d27231bed8fa190d7df0356972e2fd44ca144c084ad4fc7

printe_h2 "Installing leiningen..."

if is_force || file_absent "$HOME/.local/bin/lein"; then
    fetch_gh_raw "$HOME/.local/bin/lein.new" technomancy/leiningen $lein_ver bin/lein
    verify_shasum "$HOME/.local/bin/lein.new" $lein_sha256

    mv "$HOME/.local/bin/lein.new" "$HOME/.local/bin/lein"
    chmod 0755 "$HOME/.local/bin/lein"
    printe "leiningen has been successfully installed"
fi
