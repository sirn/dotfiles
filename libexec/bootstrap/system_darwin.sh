#!/bin/sh -e
#
# Sets up Darwin system.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root
ensure_platform "Darwin"

FLAVORS=$*
DNSCRYPT_CONF=/usr/local/etc/dnscrypt-proxy.toml


## Setup
##

_setup_dnscrypt() {
    printe_h2 "Setting up dnscrypt-proxy..."

    run_root brew services start dnscrypt-proxy

    if is_force || [ ! -f $DNSCRYPT_CONF ]; then
        run_root cp "$BOOTSTRAP_ROOT/etc/dnscrypt-proxy/dnscrypt-proxy.toml" $DNSCRYPT_CONF
        run_root chmod 0644 $DNSCRYPT_CONF
        run_root chown nobody:nobody $DNSCRYPT_CONF
        run_root brew services restart dnscrypt-proxy
    else
        printe "$DNSCRYPT_CONF already exists, not overwriting"
    fi
}


## Run
##

_run() {
    _setup_dnscrypt
}

run_with_flavors "$FLAVORS"
