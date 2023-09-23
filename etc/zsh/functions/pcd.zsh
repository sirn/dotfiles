#!/usr/bin/env zsh
#
# Change directory in a project directory.
#
pcd() {
    if [ "$#" != 0 ]; then
        builtin cd "$@" || return 1
        return
    fi

    if [ ! -d "$HOME/Projects" ]; then
        echo >&2 "No projects directory"
        return 1
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

    dir=$(
        find "$HOME/Projects" \
            -type d \
            -not -iname '.*' |
            fzf --tac --preview "$lscmd {}"
    )

    builtin cd "$dir" || return 1
}
