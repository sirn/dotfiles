#!/bin/sh

portsnap auto

if hash synth 2>/dev/null; then
    synth prepare-system
    synth purge-distfiles
fi
