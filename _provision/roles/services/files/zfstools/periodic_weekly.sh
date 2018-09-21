#!/bin/sh

if [ -f /usr/local/sbin/zfs-auto-snapshot ]; then
    /usr/local/sbin/zfs-auto-snapshot weekly 4
fi
