#!/usr/bin/env zsh
#
# Quickly jump into project directory.
#
gg() {
    local dir
    local codedirs

    codedirs=()
    if command -v git >/dev/null; then
        for s in $(git config --get-all ghq.root); do
            s=$(
                eval -- builtin cd "$s" 2>/dev/null || return 1
                pwd -P
            )
            if [ -n "$s" ]; then
                codedirs+=("${s}")
            fi
        done
    fi

    if [ "${#codedirs[*]}" -lt 1 ]; then
        codedirs[0]=$HOME/src
    fi

    # Note: command should match with `gemacs--project-sync' in
    # `etc/elisp/packages/editor-project.el'.
    dir=$(
        find "${codedirs[@]}" \
            -not -path '*/node_modules/*' \
            -not -path '*/vendor/*' \
            \( \
            -exec test -d '{}'/.git \; \
            -or -exec test -d '{}'/.hg \; \
            -or -exec test -f '{}'/.project \; \
            -or -exec test -f '{}'/.projectile \; \
            \) \
            -print \
            -prune |
            sort -u |
            fzf --layout reverse --height 40% -q "$*"
    )

    if [ -z "$dir" ]; then
        return
    fi

    builtin cd "$dir" || return 1
}
