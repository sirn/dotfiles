#!/bin/sh -e
#
# Configure current user shell.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi


## Main
##

conf_file=$(mangle_file1 "$root_dir/var/bootstrap/shell.txt" "$platform")

if [ -z "$conf_file" ]; then
    printe_info "Shell configuration not found, skipping"
    exit
fi

printe_h2 "Setting current user shell from ${conf_file##$root_dir/}..."

shell="$(cat "$conf_file")"

if [ -z "$shell" ]; then
    printe "Shell configuration seems to be empty, skipping"
    exit
fi

if ! shell_bin=$(command -v "$shell"); then
    printe_err "$shell is not a valid shell, aborting"
    exit 1
fi

if ! grep -q "$shell_bin" /etc/shells; then
    printe_info "Adding $shell_bin to /etc/shells..."
    printf "%s\\n" "$shell_bin" | run_root tee -a /etc/shells
fi

run_root chsh -s "$shell_bin" "$USER"
