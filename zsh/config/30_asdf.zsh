#!/usr/local/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | eterm* | screen* )
        if [ -d "$HOME/.asdf" ]; then
            . "$HOME/.asdf/asdf.sh"
            . "$HOME/.asdf/completions/asdf.bash"
        fi
        ;;

    * )
        ;;
esac
