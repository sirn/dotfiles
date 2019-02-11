#!/usr/local/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | screen* )
        function pet-select() {
            BUFFER=$(pet search --query "$LBUFFER")
            CURSOR=$#BUFFER
            zle redisplay
        }

        zle -N pet-select
        stty -ixon
        bindkey '^s' pet-select
        ;;

    eterm* )
        ;;

    * )
        ;;
esac
