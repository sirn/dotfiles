#!/usr/local/bin/env zsh

case "$TERM_PROGRAM" in
    Apple_Terminal )
        autoload -Uz add-zsh-hook

        _macos_update_title_hook () {
            local file_url="file://$HOSTNAME${PWD// /%20}"
            printf '\e]7;%s\a' "$file_url"
        }

        add-zsh-hook precmd _macos_update_title_hook
        ;;

    * )
        ;;
esac
