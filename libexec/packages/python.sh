#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

_run() {
    if ! command -v pip3 >/dev/null; then
       printe_h2 "Installing python3 packages..."
       printe_info "pip3 is not installed, skipping..."
       return 1
    fi
}

_run_dev() {
    pip3 install --user \
         black \
         ipwhois \
         pip \
         pipenv \
         poetry \
         pre-commit \
         proselint \
         pylint \
         virtualenv
}

run_with_flavors "$@"
