#!/bin/sh -e
#
# Install Erlang utilities
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*

REBAR3_VER=3.10.0
REBAR3_SHA256=5887a6228fec0a81d45416f53623563166d46b73b52638e6aaef6fa30d7ea5e7

REBAR3_HOME=$HOME/.cache/rebar3
REBAR3_PATH=$REBAR3_HOME/bin/rebar3


## Runs
##

_run_dev() {
    printe_h2 "Installing rebar3..."

    if is_force || file_absent "$REBAR3_PATH"; then
        mkdir -p "$(dirname "$REBAR3_PATH")"

        fetch_gh_release "$REBAR3_PATH" erlang/rebar3 $REBAR3_VER rebar3
        verify_shasum "$REBAR3_PATH" $REBAR3_SHA256
        chmod 755 "$REBAR3_PATH"

        "$REBAR3_PATH" local install
    fi
}

run_with_flavors "$FLAVORS"
