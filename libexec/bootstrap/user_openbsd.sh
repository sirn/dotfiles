#!/bin/sh -e
#
# Configure current user on OpenBSD.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Hand-off
##

"$root_dir/libexec/bootstrap/user_shell.sh" "$flavors"
"$root_dir/libexec/bootstrap/user_links.sh" "$flavors"
