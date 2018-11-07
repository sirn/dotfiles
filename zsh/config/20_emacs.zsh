#!/usr/bin/env zsh

if [ -n "$INSIDE_EMACS" ]; then
    _zsh_autosuggest_disable
fi

if hash emacsclient 2>/dev/null; then
    export EDITOR="emacsclient -c -a ''"
    alias emacs="emacsclient -c -a ''"
fi
