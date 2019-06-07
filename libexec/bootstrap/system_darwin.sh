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
DNSCRYPT_CONF=/opt/local/share/dnscrypt-proxy/dnscrypt-proxy.toml
DNSCRYPT_SUDOERS=/etc/sudoers.d/01_dnscrypt


## Setup
##

_setup_dnscrypt() {
    printe_h2 "Setting up dnscrypt-proxy..."

    run_root port load dnscrypt-proxy

    if is_force || [ ! -f $DNSCRYPT_SUDOERS ]; then
        run_root cp "$BOOTSTRAP_ROOT/share/examples/sudoers.d/01_dnscrypt.darwin" $DNSCRYPT_SUDOERS
        run_root chmod 0644 $DNSCRYPT_SUDOERS
        run_root chown root:staff $DNSCRYPT_SUDOERS
    else
        printe "$DNSCRYPT_SUDOERS already exists, not overwriting"
    fi

    if is_force || [ ! -f $DNSCRYPT_CONF ]; then
        run_root cp "$BOOTSTRAP_ROOT/etc/dnscrypt-proxy/dnscrypt-proxy.toml" $DNSCRYPT_CONF
        run_root chmod 0644 $DNSCRYPT_CONF
        run_root chown nobody:nobody $DNSCRYPT_CONF
        run_root port unload dnscrypt-proxy
        run_root port load dnscrypt-proxy
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
