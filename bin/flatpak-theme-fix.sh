#!/bin/sh
#
# Based on abiosoft's fixflatpaktheme.sh
# https://github.com/abiosoft/dotfiles/blob/master/flatpak/fixflatpaktheme.sh
#

for appdir in $HOME/.var/app/*; do
    if [ ! -d "$appdir" ]; then
        continue
    fi

    printf >&2 "==> Fixing %s...\\n" "$(basename "$appdir")"

    for file in gtk-3.0/settings.ini fontconfig/fonts.conf; do
        if [ -f "$HOME/.config/$file" ]; then
            mkdir -p "$appdir/config/$(dirname "$file")"
            cp "$HOME/.config/$file" "$appdir/config/$file"
        fi
    done
done
