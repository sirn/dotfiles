#!/bin/sh -e
#
# Sets up Darwin system.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a Darwin system"
    exit 1
fi


## Dnscrypt-proxy
##

printe_h2 "Setting up dnscrypt-proxy..."

dnscrypt_conf="/usr/local/etc/dnscrypt-proxy.toml"
dnscrypt_started="$(service_running dnscrypt-proxy)"
dnscrypt_updated=0

if [ "$(normalize_bool "$FORCE")" = "1" ] || [ ! -f "$dnscrypt_conf" ]; then
    run_root cp ../../share/examples/bootstrap/dnscrypt-proxy.toml "$dnscrypt_conf"
    run_root chmod 0644 "$dnscrypt_conf"
    run_root chown nobody:nobody "$dnscrypt_conf"
    dnscrypt_updated=1
else
    printe "$dnscrypt_conf already exists, not overwriting"
fi

if [ "$dnscrypt_updated" = "1" ]; then
    dnscrypt_cmd="start"

    if [ "$dnscrypt_started" = "1" ]; then
        dnscrypt_cmd="restart"
    fi

    run_root brew services "$dnscrypt_cmd" dnscrypt-proxy
fi
