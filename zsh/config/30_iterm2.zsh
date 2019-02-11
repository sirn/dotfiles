#!/usr/local/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | screen* )
        if [ -f "$HOME/.iterm2_shell_integration.zsh" ]; then
            . "$HOME/.iterm2_shell_integration.zsh"
        fi
        ;;

    eterm* )
        ;;

    *)
        ;;
esac
