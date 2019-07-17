#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

PIP=

_run() {
    PIP=$(detect_pip3)

    if [ -z "$PIP" ]; then
       printe_h2 "pip3 is not installed, skipping python packages..."
       return 1
    fi
}

_run_dev() {
    printe_h2 "Installing python dev packages..."

    $PIP install --user \
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
