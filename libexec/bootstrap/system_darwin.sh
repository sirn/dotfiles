#!/bin/sh -e
#
# Sets up Darwin system.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a Darwin system"
    exit 1
fi


## Dnscrypt-proxy
##

printe_h2 "Setting up dnscrypt-proxy..."

run_root brew services start dnscrypt-proxy

dnscrypt_conf=/usr/local/etc/dnscrypt-proxy.toml

if is_force || [ ! -f $dnscrypt_conf ]; then
    run_root cp "$root_dir/etc/dnscrypt-proxy/dnscrypt-proxy.toml" $dnscrypt_conf
    run_root chmod 0644 $dnscrypt_conf
    run_root chown nobody:nobody $dnscrypt_conf
    run_root brew services restart dnscrypt-proxy
else
    printe "$dnscrypt_conf already exists, not overwriting"
fi
