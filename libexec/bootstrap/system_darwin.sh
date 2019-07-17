#!/bin/sh -e
#
# Sets up Darwin system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_darwin.sh"

_setup_dnscrypt() {
    printe_h2 "Setting up dnscrypt-proxy..."

    dnscrypt_conf=/opt/local/share/dnscrypt-proxy/dnscrypt-proxy.toml

    if ! forced && [ -f $dnscrypt_conf ]; then
        printe_info "$dnscrypt_conf already exists, skipping..."
        return
    fi

    run_root cp \
             "$BASE_DIR/etc/dnscrypt-proxy/dnscrypt-proxy.toml" \
             $dnscrypt_conf

    run_root chmod 0644 $dnscrypt_conf
    run_root chown nobody:nobody $dnscrypt_conf
    run_root port unload dnscrypt-proxy
    run_root port load dnscrypt-proxy
}

_setup_dnscrypt_sudoers() {
    printe_h2 "Setting up dnscrypt-proxy sudoers..."

    dnscrypt_sudoers=/etc/sudoers.d/01_dnscrypt

    if ! forced && [ -f $dnscrypt_sudoers ]; then
        printe_info "$dnscrypt_sudoers already exists, skipping..."
        return
    fi

    run_root cp \
             "$BASE_DIR/share/examples/sudoers.d/01_dnscrypt.darwin" \
             $dnscrypt_sudoers

    run_root chmod 0644 $dnscrypt_sudoers
    run_root chown root:staff $dnscrypt_sudoers
}

_run() {
    _setup_dnscrypt
    _setup_dnscrypt_sudoers
}

run_with_flavors "$@"
