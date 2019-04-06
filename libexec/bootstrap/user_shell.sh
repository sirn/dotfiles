#!/bin/sh -e
#
# Configure current user shell on FreeBSD.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh


## Shell
##

shellspec="../../var/bootstrap/shell.txt"

printe_h2 "Setting current user shell..."

if [ ! -f "$shellspec" ]; then
    printe_msg "Shell spec is not configured, skipping"
    exit
fi

shell="$(cat "$shellspec")"

if [ -z "$shell" ]; then
    printe_msg "Shell spec is not configured, skipping"
    exit
fi

if ! hash "$shell" 2>/dev/null; then
    printe_err "$shell is not a valid shell, aborting"
    exit 1
fi

shell_bin="$(command -v "$shell")"

if ! grep -q "$shell_bin" /etc/shells; then
    printe_msg "Adding $shell_bin to /etc/shells..."
    printf "%s\\n" "$shell_bin" | run_root tee -a /etc/shells > /dev/null
fi

run_root chsh -s "$shell_bin" "$USER"
