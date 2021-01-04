#!/bin/sh -e
#
# Configure current user on Darwin.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./user_darwin-amd64.sh"
