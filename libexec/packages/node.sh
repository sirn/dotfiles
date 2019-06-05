#!/bin/sh -e
#
# Install node packages
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')

NODE_PKGLIST=$LOOKUP_ROOT/var/bootstrap/pkglist_node.txt


## Environment variables
##

case $PLATFORM in
    darwin )
        PATH=/usr/local/opt/node@10/bin:$PATH
        ;;

    * )
        ;;
esac


## Run
##

_run() {
    if command -v npm >/dev/null; then
       npm set prefix="$HOME/.local"

       for f in $(mangle_file "$NODE_PKGLIST" "$PLATFORM" "$FLAVORS"); do
           printe_h2 "Installing node packages from $f..."
           xargs npm install -g < "$f"
       done
    fi
}

run_with_flavors "$FLAVORS"
