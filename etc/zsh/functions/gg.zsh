#!/usr/bin/env zsh
#
# Quickly jump into project directory.
#
gg() {
    dir=$($HOME/.dotfiles/bin/pom list | fzf --layout reverse --height 40% -q "$*")
    if [ -z "$dir" ]; then
        return
    fi

    builtin cd "$dir" || return 1
}
