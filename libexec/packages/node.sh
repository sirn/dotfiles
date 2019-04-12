#!/bin/sh -e
#
# Install node packages
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


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

node_pkglist=../../var/bootstrap/pkglist_node.txt

if command -v npm >/dev/null; then
   npm set prefix="$HOME/.local"

   for f in $(mangle_file $node_pkglist "$platform" "$flavors"); do
       printe_h2 "Installing node packages from ${f##../../}..."
       xargs npm install -g < "$f"
   done
fi
