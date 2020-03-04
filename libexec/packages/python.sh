#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

PYTOOLS=$HOME/.local/lib/pytools
PIP=$PYTOOLS/bin/pip3

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

    if [ -f "$PIP" ]; then
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

    $PIP install --upgrade \
         ansible \
         black \
         flake8 \
         ipwhois \
         kapitan \
         pip \
         poetry \
         pre-commit \
         proselint \
         pyls-black \
         pyls-isort \
         pyls-mypy \
         python-language-server[flake8] \
         virtualenv
}

run_with_flavors "$@"
