#!/bin/sh
#
# Check whether there is any update in pkgbuild directory.
# Based on https://stackoverflow.com/a/3278427
#

PKGBUILD_ROOT=${XDG_DATA_HOME:-$HOME/.local/share}/pkgbuild

tput_green() {
    tput setaf 10 >&2
}

tput_red() {
    tput setaf 9 >&2
}

tput_yellow() {
    tput setaf 11 >&2
}

tput_purple() {
    tput setaf 13 >&2
}

tput_reset() {
    tput sgr0 >&2
}

for pkgdir in "$PKGBUILD_ROOT"/*; do
    pkgdir=$(cd "$pkgdir" || exit 1; pwd -P)
    pkgname=$(basename "$pkgdir")

    if [ ! -d "$pkgdir"/.git ]; then
        echo >&2 "$pkgdir is not a git-controlled directory"
        continue
    fi

    printf >&2 "Checking update for %s... " $pkgname
    git -C "$pkgdir" remote update -p >/dev/null 2>&1

    _upstream="@{u}"
    _revlocal=$(git -C "$pkgdir" rev-parse @)
    _revremote=$(git -C "$pkgdir" rev-parse @{u})
    _revbase=$(git -C "$pkgdir" merge-base @ @{u})

    if [ "$revlocal" = "$revremote" ]; then
        tput_green
        printf >&2 "up-to-date"
    elif [ "$revlocal" = "$revbase" ]; then
        tput_red
        printf >&2 "outdated"
    elif [ "$revremote" = "$revbase" ]; then
        tput_yellow
        printf >&2 "ahead"
    else
        tput_purple
        printf >&2 "diverged"
    fi

    tput_reset
    printf >&2 "\\n"
done
