#!/bin/sh -e
#
# Sets up Darwin system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

_setup_dnscrypt() {
    printe_h2 "Setting up dnscrypt-proxy..."

    dnscrypt_conf=/opt/local/share/dnscrypt-proxy/dnscrypt-proxy.toml
    dnscrypt_sudoers=/etc/sudoers.d/01_dnscrypt

    if is_force || [ ! -f $dnscrypt_sudoers ]; then
        run_root cp \
                 "$BASE_DIR/share/examples/sudoers.d/01_dnscrypt.darwin" \
                 $dnscrypt_sudoers

        run_root chmod 0644 $dnscrypt_sudoers
        run_root chown root:staff $dnscrypt_sudoers
    else
        printe_info "$dnscrypt_sudoers already exists, not overwriting"
    fi

    if is_force || [ ! -f $dnscrypt_conf ]; then
        run_root cp \
                 "$BASE_DIR/etc/dnscrypt-proxy/dnscrypt-proxy.toml" \
                 $dnscrypt_conf

        run_root chmod 0644 $dnscrypt_conf
        run_root chown nobody:nobody $dnscrypt_conf
        run_root port unload dnscrypt-proxy
        run_root port load dnscrypt-proxy
    else
        printe_info "$dnscrypt_conf already exists, not overwriting"
    fi

    run_root port load dnscrypt-proxy
}

_run() {
    _setup_dnscrypt
}

run_with_flavors "$@"
