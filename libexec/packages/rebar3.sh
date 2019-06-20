#!/bin/sh -e
#
# Install rebar3
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

ESCRIPT=
ERL=

REBAR3_VER=3.10.0
REBAR3_SHA256=5887a6228fec0a81d45416f53623563166d46b73b52638e6aaef6fa30d7ea5e7
REBAR3_HOME=$HOME/.cache/rebar3
REBAR3_PATH=$REBAR3_HOME/bin/rebar3

_run() {
    printe_h2 "Installing rebar3..."

    for ver in 22 21 20 ""; do
        if command -v erl$ver >/dev/null; then
           ESCRIPT=escript$ver
           ERL=erl$ver
           break
        fi
    done

    if [ -z "$ESCRIPT" ]; then
        printe_info "escript is not installed, skipping..."
        return
    fi

    if [ -z "$ERL" ]; then
        printe_info "erl is not installed, skipping..."
        return
    fi

    if is_force || file_absent "$REBAR3_PATH"; then
        mkdir -p "$(dirname "$REBAR3_PATH")"

        fetch_gh_release "$REBAR3_PATH" erlang/rebar3 $REBAR3_VER rebar3
        verify_shasum "$REBAR3_PATH" $REBAR3_SHA256
        chmod 755 "$REBAR3_PATH"

        "$ESCRIPT" "$REBAR3_PATH" local install

        sed "s|erl|$ERL|" "$REBAR3_PATH" > "$REBAR3_PATH.new"
        mv "$REBAR3_PATH.new" "$REBAR3_PATH"
        chmod +x "$REBAR3_PATH"
    fi
}

_run
