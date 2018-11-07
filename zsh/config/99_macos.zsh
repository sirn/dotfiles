#!/usr/local/bin/env zsh

[ -n "$INSIDE_EMACS" ] && return

if [ "$TERM_PROGRAM" == "Apple_Terminal" ]; then
    autoload -Uz add-zsh-hook

    _macos_update_title_hook () {
        local file_url="file://$HOSTNAME${PWD// /%20}"
        printf '\e]7;%s\a' "$file_url"
    }

    add-zsh-hook precmd _macos_update_title_hook
fi
