#!/bin/sh -e
#
# Sets up Darwin system.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a Darwin system"
    exit 1
fi


## Dnscrypt-proxy
##

printe_h2 "Setting up dnscrypt-proxy..."

run_root brew services start dnscrypt-proxy

dnscrypt_conf=/usr/local/etc/dnscrypt-proxy.toml

if normalize_bool "$FORCE" || [ ! -f $dnscrypt_conf ]; then
    run_root cp ../../share/examples/bootstrap/dnscrypt-proxy.toml $dnscrypt_conf
    run_root chmod 0644 $dnscrypt_conf
    run_root chown nobody:nobody $dnscrypt_conf
    run_root brew services restart dnscrypt-proxy
else
    printe "$dnscrypt_conf already exists, not overwriting"
fi
