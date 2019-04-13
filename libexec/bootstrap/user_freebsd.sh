#!/bin/sh -e
#
# Configure current user on FreeBSD.
#

root_dir=${BOOTSTRAP_ROOT:-../../}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Hand-off
##

"$root_dir/libexec/bootstrap/user_shell.sh" "$flavors"
"$root_dir/libexec/bootstrap/user_links.sh" "$flavors"
