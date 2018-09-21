#!/bin/sh

if [ -f /usr/local/sbin/zfs-auto-snapshot ]; then
    /usr/local/sbin/zfs-auto-snapshot monthly 6
fi
