#!/usr/local/bin/env zsh

[ -n "$INSIDE_EMACS" ] && return

if [ -f "$HOME/.iterm2_shell_integration.zsh" ]; then
    . "$HOME/.iterm2_shell_integration.zsh"
fi
