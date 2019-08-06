#!/bin/sh -e
#
# Sets up FreeBSD system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_freebsd.sh"
. "../../share/bootstrap/buildenv.sh"

_setup_pf() {
    printe_h2 "Setting up pf..."

    if ! forced && [ -f /etc/pf.conf ]; then
        printe_info "/etc/pf.conf already exists, skipping..."
        return
    fi

    for file in pf.conf pf.trusted; do
        if [ ! -f /usr/local/etc/$file ]; then
            run_root touch /usr/local/etc/$file
            run_root chown root:wheel /usr/local/etc/$file
            run_root chmod 0600 /usr/local/etc/$file
            printe_info "/usr/local/etc/$file successfully created"
        fi
    done

    netif=$(get_netif)
    sshd_port=$(get_sshd_port)

    run_root cp "$BASE_DIR/etc/pf/pf.freebsd.conf" /etc/pf.conf.new

    lineinfile \
        -S \
        -f /etc/pf.conf.new \
        -l "ext_if=$netif" \
        -r "^ext_if=" \
        -s present

    lineinfile \
        -S \
        -f /etc/pf.conf.new \
        -l "sshd_port=$sshd_port" \
        -r "^sshd_port=" \
        -s present

    run_root mv /etc/pf.conf.new /etc/pf.conf
    run_root chown root:wheel /etc/pf.conf
    run_root chmod 0600 /etc/pf.conf

    run_root sysrc pf_enable=YES
    run_root service pf onestart

    if ! run_root pfctl -nf /etc/pf.conf; then
        printe_err "\
/etc/pf.conf has been updated but contained errors, exiting\
"
        exit 1
    fi

    run_root pfctl -F all -f /etc/pf.conf
    printe_info "\
/etc/pf.conf has been updated, internet connection might be interrupted\
"
}

_run() {
    _setup_pf
}

run_with_flavors "$@"
