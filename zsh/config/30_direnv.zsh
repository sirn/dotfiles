#!/usr/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | eterm* | screen* )
        if hash direnv 2>/dev/null; then
            eval "$(direnv hook zsh)"
        fi
        ;;

    * )
        ;;
esac
