#!/bin/sh
#
# Recompile Emacs source in dotfiles directory.
#

BASE_DIR=$(cd "$(dirname "$0")/.." || exit; pwd -P)
PATH=/usr/bin:/usr/local/bin

find "$BASE_DIR/etc/emacs" -iname '*.elc' -delete
emacs --batch \
      --load "$BASE_DIR/etc/emacs/init.el" \
      --eval '(byte-recompile-directory "'"$BASE_DIR"'/etc/emacs/packages" 0)'
