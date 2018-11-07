#!/usr/local/bin/env zsh

if [ -d "$ZPLUG_REPOS/junegunn/fzf" ]; then
    . "$ZPLUG_REPOS/junegunn/fzf/shell/key-bindings.zsh"
    . "$ZPLUG_REPOS/junegunn/fzf/shell/completion.zsh"
fi
