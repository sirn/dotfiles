#!/bin/sh -e
#
# Configure current user on FreeBSD.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Hand-off
##

"$base_dir/user_shell.sh" "$flavors"
"$base_dir/user_links.sh" "$flavors"
