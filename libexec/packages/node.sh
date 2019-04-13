#!/bin/sh -e
#
# Install node packages
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Setup environment
##

case $platform in
    darwin )
        PATH=/usr/local/opt/node@10/bin:$PATH
        ;;

    * )
        ;;
esac


## Setup
##

node_pkglist=$lookup_dir/var/bootstrap/pkglist_node.txt

if command -v npm >/dev/null; then
   npm set prefix="$HOME/.local"

   for f in $(mangle_file "$node_pkglist" "$platform" "$flavors"); do
       printe_h2 "Installing node packages from ${f##$lookup_dir/}..."
       xargs npm install -g < "$f"
   done
fi
