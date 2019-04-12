#!/bin/sh -e
#
# Install kubectx.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Setup
##

printe_h2 "Installing kubectx..."

git_clone_update https://github.com/ahmetb/kubectx.git "$HOME/.local/src/kubectx"
make_link "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
make_link "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"
