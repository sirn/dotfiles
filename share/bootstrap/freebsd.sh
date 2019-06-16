#!/bin/sh -e
#
# Shared functions for FreeBSD
#

## Package installation
##

_do_pkgng() {
    pkg=$1; shift
    pkg install -y "$pkg"
}
