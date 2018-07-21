#!/bin/sh

if [ -d /usr/ports ]; then
    portsnap cron
    portsnap fetch
    portsnap update
fi
