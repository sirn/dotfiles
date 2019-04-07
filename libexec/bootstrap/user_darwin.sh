#!/bin/sh -e
#
# Configure current user on Darwin.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Userenv
##

printe_h2 "Setting up userenv..."

userenv_plist="$HOME/Library/LaunchAgents/th.in.grid.userenv.plist"

if [ "$(normalize_bool "$FORCE")" = "1" ] || [ ! -f "$userenv_plist" ]; then
    cp ../../share/examples/bootstrap/th.in.grid.userenv.plist "$userenv_plist"
    chmod 0644 "$userenv_plist"
    launchctl load -w "$userenv_plist"
    printe "$userenv_plist has been installed, you may need to relogin"
else
    printe "$userenv_plist already exists"
fi


## Hand-off
##

"$base_dir/user_shell.sh" "$flavors"
"$base_dir/user_links.sh" "$flavors"


## Chunkwm/Skhd
## must be run after link
##

if [ "$(has_args "desktop" "$flavors")" = "1" ]; then
    if [ "$(service_running chunkwm)" != "1" ]; then
        printe_h2 "Setting up chunkwm..."
        brew services start chunkwm
    fi

    if [ "$(service_running skhd)" != "1" ]; then
        printe_h2 "Setting up skhd..."
        brew services start skhd
    fi
fi
