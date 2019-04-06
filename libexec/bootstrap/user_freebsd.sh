#!/bin/sh -e
#
# Configure current user on FreeBSD.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh


## Hand-off
##

"$base_dir/user_shell.sh" "$flavors"
"$base_dir/user_links.sh" "$flavors"
