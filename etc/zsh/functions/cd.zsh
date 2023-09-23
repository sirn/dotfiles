#!/usr/bin/env zsh
#
# Change Directory with selection menu.
#
cd() {
    if [ "$#" != 0 ]; then
        builtin cd "$@" || return 1
        return
    fi

    local lscmd
    local dir

    if command -v colorls >/dev/null; then
        lscmd="command colorls -G -p"
    elif command ls --version 2>&1 | grep -qe "BusyBox" -e "GNU"; then
        lscmd="command ls --color -p"
    elif command ls -G >/dev/null 2>&1; then
        lscmd="command ls -Gp"
    fi

    while true; do
        dir=$(command ls -a -p |
            grep '/$' |
            sed 's;/$;;' |
            fzf --tac --preview "$lscmd {}")

        if [ -z "$dir" ]; then
            return
        fi

        builtin cd "$dir" || return 1
    done
}
