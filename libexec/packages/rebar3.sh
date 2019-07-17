#!/bin/sh -e
#
# Install rebar3
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

REBAR3_VER=3.10.0
REBAR3_SHA256=5887a6228fec0a81d45416f53623563166d46b73b52638e6aaef6fa30d7ea5e7
REBAR3_HOME=$HOME/.cache/rebar3
REBAR3_PATH=$REBAR3_HOME/bin/rebar3

_run() {
    printe_h2 "Installing rebar3..."

    if ! forced && [ -f "$REBAR3_PATH" ]; then
        printe_info "$REBAR3_PATH already exists, skipping..."
        return
    fi

    if ! command -v escript >/dev/null; then
        printe_info "escript is not installed, skipping..."
        return
    fi

    mkdir -p "$(dirname "$REBAR3_PATH")"

    fetch_gh_release "$REBAR3_PATH" erlang/rebar3 $REBAR3_VER rebar3
    verify_shasum "$REBAR3_PATH" $REBAR3_SHA256
    chmod 755 "$REBAR3_PATH"

    escript "$REBAR3_PATH" local install
}

_run
