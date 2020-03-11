#!/bin/sh

PATH=/usr/bin:/usr/local/bin

find "$HOME/.dotfiles/etc/emacs" -iname '*.elc' -delete
emacs --batch \
      --load "$HOME/.emacs.d/init.el" \
      --eval '(byte-recompile-directory "~/.dotfiles/etc/emacs/packages" 0)'
