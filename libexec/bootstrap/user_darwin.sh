#!/bin/sh -e
#
# Configure current user on Darwin.
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

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Userenv
##

printe_h2 "Setting up userenv..."

userenv_plist="$HOME/Library/LaunchAgents/th.in.grid.userenv.plist"

if normalize_bool "$FORCE" || [ ! -f "$userenv_plist" ]; then
    cp "$root_dir/share/examples/launchd/th.in.grid.userenv.plist" "$userenv_plist"
    chmod 0644 "$userenv_plist"
    launchctl load -w "$userenv_plist"
    printe "$userenv_plist has been installed, you may need to relogin"
else
    printe "$userenv_plist already exists"
fi


## Hand-off
##

"$root_dir/libexec/bootstrap/user_shell.sh" "$flavors"
"$root_dir/libexec/bootstrap/user_links.sh" "$flavors"


## Chunkwm/Skhd
## must be run after link
##

if has_args "desktop" "$flavors"; then
    printe_h2 "Setting up chunkwm..."
    brew services start chunkwm

    printe_h2 "Setting up skhd..."
    brew services start skhd
fi
