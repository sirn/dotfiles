#!/bin/sh
#
# Thaw straight versions in dotfiles directory.
#

BASE_DIR=$(cd "$(dirname "$0")/.." || exit; pwd -P)
PATH=/usr/bin:/usr/local/bin

emacs --batch --load "$BASE_DIR/etc/emacs/init.el" --eval '(straight-thaw-versions)'
emacs --batch --load "$BASE_DIR/etc/emacs/init.el" --eval '(straight-rebuild-all)'
