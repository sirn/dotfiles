#!/bin/sh -e
#
# Based on a Reddit thread: https://www.reddit.com/r/openbsd/comments/5ab8y7/
#
# This is a script for updating OpenBSD's bsd.rd to latest snapshot.
# After this script successfully fetch the latest bsd.rd, the following steps
# are required to completely upgrade the OpenBSD system:
#
# 1. Reboot the machine
# 2. At the boot> prompt, enter bsd.rd
# 3. Choose upgrade and follow the upgrade steps
# 4. Reboot the machine
# 5. Run: sysmerge && pkg_add -u
#

if [ "$(id -u)" != "0" ]; then
    printf 2>&1 "Must be run as root\\n"
    exit 1
fi

_mirror_url="https://cloudflare.cdn.openbsd.org/pub/OpenBSD/snapshots/$(uname -m)"
_tmp="$(mktemp -d)"

trap 'rm -rf $_tmp' 0 1 2 3 6 14 15
cd "$_tmp" || exit 1

for filename in bsd.rd SHA256 SHA256.sig; do
    ftp -o "$filename" "$_mirror_url/$filename"
done

_key1="$(find /etc/signify/openbsd-??-base.pub | tail -2 | head -1)"
_key2="$(find /etc/signify/openbsd-??-base.pub | tail -2 | tail -1)"
(signify -C -p "$_key1" -x SHA256.sig bsd.rd || signify -C -p "$_key2" -x SHA256.sig bsd.rd) || exit 1

mv bsd.rd /
