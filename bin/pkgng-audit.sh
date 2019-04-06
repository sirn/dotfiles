#!/bin/sh
#
# This is a script for auditing FreeBSD packages that are marked as
# manually installed.
#

for pkg in $(pkg query -e '%a = 0' %o); do
    sudo pkg set -A 1 "$pkg"
done
