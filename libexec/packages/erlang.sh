#!/bin/sh -e
#
# Install Erlang utilities
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Preparation
##

rebar3_ver=3.10.0
rebar3_sha256=5887a6228fec0a81d45416f53623563166d46b73b52638e6aaef6fa30d7ea5e7

printe_h2 "Installing rebar3..."


## Setup
##

rebar3_home=$HOME/.cache/rebar3
rebar3_path=$rebar3_home/bin/rebar3

if is_force || file_absent "$rebar3_path"; then
    mkdir -p "$(dirname "$rebar3_path")"

    fetch_gh_release "$rebar3_path" erlang/rebar3 $rebar3_ver rebar3
    verify_shasum "$rebar3_path" $rebar3_sha256
    chmod 755 "$rebar3_path"

    "$rebar3_path" local install
fi
