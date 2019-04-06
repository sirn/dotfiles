#!/bin/sh -e
#
# Configure current user shell.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh


## Utils
##

_find_conf() {
    pathlist=$*; shift

    for path in $pathlist; do
        if [ -f "$path" ]; then
            printf "%s\\n" "$path"
            return
        fi
    done
}


## Main
##

conf_dir="../../var/bootstrap"
conf_file=$(_find_conf "$conf_dir/${platform}/shell.txt" "$conf_dir/shell.txt")

printe_h2 "Setting current user shell..."

if [ -z "$conf_file" ] || [ ! -f "$conf_file" ]; then
    printe_info "Shell configuration could not be found, skipping"
    exit
fi


printe_info "Using shell configuration defined in ${conf_file##../../}"

shell="$(cat "$conf_file")"

if [ -z "$shell" ]; then
    printe "Shell configuration seems to be empty, skipping"
    exit
fi

if ! hash "$shell" 2>/dev/null; then
    printe_err "$shell is not a valid shell, aborting"
    exit 1
fi


shell_bin="$(command -v "$shell")"

if ! grep -q "$shell_bin" /etc/shells; then
    printe_info "Adding $shell_bin to /etc/shells..."
    printf "%s\\n" "$shell_bin" | run_root tee -a /etc/shells > /dev/null
fi

run_root chsh -s "$shell_bin" "$USER"
