#!/bin/sh

PATH=/usr/bin:/usr/local/bin

emacs --batch --load "$HOME/.emacs.d/init.el" --eval '(straight-freeze-versions)'
