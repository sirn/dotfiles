#!/usr/bin/env zsh

alias ls="ls -G"
alias ll="ls -lh"
alias la="ls -alh"
alias ka="k -hA"
alias kk="k -h"

if hash nvim 2>/dev/null; then
    alias nvi="nvim"
    alias vi="nvim"
    alias vim="nvim"
fi

if hash hub 2>/dev/null; then
    alias git="hub"
fi

if hash emacsclient 2>/dev/null; then
    export EDITOR="emacsclient -c -a ''"
    alias emacs="emacsclient -c -a ''"
fi
