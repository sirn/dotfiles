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

    local dir

    dir=$(
        find "$HOME/Projects" \
            -type d \
            -not -iname '.*' |
            fzf --layout reverse --height 40%
    )

    builtin cd "$dir" || return 1
}
