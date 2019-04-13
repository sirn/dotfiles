#!/bin/sh -e
#
# Configure current user on OpenBSD.
#

root_dir=${BOOTSTRAP_ROOT:-../../}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Hand-off
##

"$root_dir/libexec/bootstrap/user_shell.sh" "$flavors"
"$root_dir/libexec/bootstrap/user_links.sh" "$flavors"
