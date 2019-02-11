#!/usr/local/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | screen* )
        autoload -Uz add-zsh-hook

        _window_update_title_hook () {
            local _window_title="\e]0;$(pwd)\a"
            printf "$_window_title"
        }

        add-zsh-hook precmd _window_update_title_hook
        ;;

    eterm* )
        ;;

    * )
        ;;
esac
