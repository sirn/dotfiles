#!/bin/sh
#
# Wrapper for Emacsclient to launch Emacs automatically.
# Intended to be used as EDITOR.
#

exec emacsclient -t -c -a '' "$@"
