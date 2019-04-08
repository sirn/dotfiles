#!/bin/sh -e
#
# Configure current user on OpenBSD.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Hand-off
##

"$base_dir/user_shell.sh" "$flavors"
"$base_dir/user_links.sh" "$flavors"
