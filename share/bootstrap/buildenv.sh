#!/bin/sh -e
#
# Sets up build environment
#

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

GOPATH="$BUILD_DIR/go"
PATH="$GOPATH/bin:$PATH"

export BUILD_DIR
export GOPATH
export PATH
