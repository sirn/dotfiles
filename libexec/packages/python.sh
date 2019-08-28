#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

PYTOOLS=${XDG_DATA_HOME:-$HOME/.local/share}/pytools

_run() {
    if ! command -v python3 >/dev/null; then
       printe_h2 "python3 is not installed, skipping python packages..."
       return 1
    fi

    if ! python3 -c 'import venv' 2>/dev/null; then
       printe_h2 "python3 venv is not available, skipping python packages..."
       return 1
    fi

    printe_h2 "Populating $PYTOOLS..."

    if [ -f "$PYTOOLS/bin/pip3" ]; then
       printe_info "$PYTOOLS already exists, skipping..."
       return
    fi

    python3 -m venv --without-pip "$PYTOOLS"
    fetch_url /tmp/get-pip.py https://bootstrap.pypa.io/get-pip.py
    "$PYTOOLS/bin/python3" /tmp/get-pip.py
    rm /tmp/get-pip.py
}

_run_dev() {
    printe_h2 "Installing python dev packages..."

    "$PYTOOLS/bin/pip3" install \
         black \
         ipwhois \
         pip \
         poetry \
         pre-commit \
         proselint \
         virtualenv
}

run_with_flavors "$@"
