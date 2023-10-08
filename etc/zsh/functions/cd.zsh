#!/usr/bin/env zsh
#
# Change Directory with selection menu.
#
cd() {
    if [ "$#" != 0 ]; then
        builtin cd "$@" || return 1
        return
    fi

    local dir

    while true; do
        dir=$(command ls -a -p |
            grep '/$' |
            sed 's;/$;;' |
            fzf --layout reverse --height 40%)

        if [ -z "$dir" ]; then
            return
        fi

        builtin cd "$dir" || return 1
    done
}
