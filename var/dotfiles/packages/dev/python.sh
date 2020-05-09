#!/bin/sh -e
#
# Install python packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../lib/utils.sh"
. "../../lib/buildenv.sh"

PYTOOLS3=$HOME/.local/lib/pytools3
PYTOOLS2=$HOME/.local/lib/pytools2
PIP3=$PYTOOLS3/bin/pip3

_preflight() {
    if ! command -v python3 >/dev/null; then
       printe_h2 "python3 is not installed, skipping python packages..."
       return 1
    fi

    if ! python3 -c 'import venv' 2>/dev/null; then
       printe_h2 "python3 venv is not available, skipping python packages..."
       return 1
    fi
}

_run() {
    _install_pytools3
    _install_pytools3_packages
    _install_pytools2
}

_run_dev() {
    printe_h2 "Installing python dev packages..."

    $PIP3 install --upgrade \
         black \
         buildstream \
         buildstream-external \
         flake8 \
         kapitan \
         pip \
         poetry \
         pre-commit \
         proselint \
         pygobject \
         pyls-black \
         pyls-isort \
         pyls-mypy \
         python-language-server[flake8]
}

_install_pytools3() {
    printe_h2 "Populating $PYTOOLS3..."

    if ! forced && [ -f "$PIP3" ]; then
       printe_info "$PYTOOLS3 already exists, skipping..."
       return
    fi

    python3 -m venv \
            --clear \
            --without-pip \
            "$PYTOOLS3"

    fetch_url /tmp/get-pip.py https://bootstrap.pypa.io/get-pip.py
    "$PYTOOLS3/bin/python3" /tmp/get-pip.py
    rm /tmp/get-pip.py
}

_install_pytools3_packages() {
    if [ -f "$PIP3" ]; then
       printe_info "$PYTOOLS3 already exists, skipping..."
       return
    fi

    $PIP3 install --upgrade virtualenv
}

_install_pytools2() {
    printe_h2 "Populating $PYTOOLS2..."

    if ! command -v python2 >/dev/null; then
       printe_h2 "python2 is not installed, skipping pytools2..."
       return
    fi

    if [ ! -f "$PYTOOLS3/bin/virtualenv" ]; then
       printe_info "$PYTOOLS3 has not been bootstrapped, skipping..."
       return
    fi

    if ! forced && [ -f "$PYTOOLS2/bin/pip" ]; then
       printe_info "$PYTOOLS2 already exists, skipping..."
       return
    fi

    "$PYTOOLS3/bin/virtualenv" \
        -p python2 \
        --without-pip \
        "$PYTOOLS2"

    fetch_url /tmp/get-pip.py https://bootstrap.pypa.io/get-pip.py
    "$PYTOOLS2/bin/python2" /tmp/get-pip.py
    rm /tmp/get-pip.py
}

run_with_flavors "$@"
