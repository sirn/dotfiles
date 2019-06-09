#!/bin/sh -e
#
# Sets up FreeBSD system.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root
ensure_platform "FreeBSD"

FLAVORS=$*
BUILD_DIR=$(make_temp)


## Utils
##

_get_netif() {
    netif=$(ifconfig | awk '! /^lo|^pf|^enc|^\t/ { FS=":"; print $1; exit }')

    if ! ifconfig "$netif" >/dev/null 2>&1; then
        printe_err "Could not determine primary network interface"
        exit 1
    fi

    echo "$netif"
}

_get_sshd_port() {
    sshd_config=/etc/ssh/sshd_config

    if [ ! -f $sshd_config ]; then
        printe_err "sshd configuration could not be found"
        exit 1
    fi

    sshd_port=$(awk '/^#? ?Port/ { print $NF }' < $sshd_config)

    if [ -z "$sshd_port" ]; then
        printe_err "Could not determine sshd port"
        exit 1
    fi

    echo "$sshd_port"
}


## Setup
##

_setup_pf() {
    printe_h2 "Setting up pf..."

    pf_updated=0

    for file in pf.conf pf.trusted; do
        if file_absent /usr/local/etc/$file; then
            run_root touch /usr/local/etc/$file
            run_root chown root:wheel /usr/local/etc/$file
            run_root chmod 0600 /usr/local/etc/$file
            printe "/usr/local/etc/$file successfully created"
        fi
    done

    if is_force || [ ! -f /etc/pf.conf ]; then
        netif=$(_get_netif)
        sshd_port=$(_get_sshd_port)

        run_root cp "$BOOTSTRAP_ROOT/etc/pf/pf.freebsd.conf" /etc/pf.conf

        lineinfile \
            -S \
            -f /etc/pf.conf \
            -l "ext_if=$netif" \
            -r "^ext_if=" \
            -s present

        lineinfile \
            -S \
            -f /etc/pf.conf \
            -l "sshd_port=$sshd_port" \
            -r "^sshd_port=" \
            -s present

        run_root chown root:wheel /etc/pf.conf
        run_root chmod 0600 /etc/pf.conf
        pf_updated=1
    else
        printe "/etc/pf.conf already exists, not overwriting"
    fi

    run_root sysrc pf_enable=YES
    run_root service pf onestart

    if [ $pf_updated = "1" ]; then
        if ! run_root pfctl -nf /etc/pf.conf; then
            printe_err "/etc/pf.conf has been updated but contained errors, exiting"
            exit 1
        fi

        run_root pfctl -F all -f /etc/pf.conf
        printe "/etc/pf.conf has been updated, internet connection might be interrupted"
    fi
}

_setup_nfsd() {
    printe_h2 "Setting up nfsd..."

    if [ -f /etc/exports ]; then
        netif=$(_get_netif)

        pfnfsd=/usr/local/libexec/pfnfsd.sh
        nfscron=/usr/local/libexec/pfnfsd_periodic.sh

        run_root sysrc nfs_server_enable=YES
        run_root sysrc mountd_enable=YES
        run_root sysrc rpcbind_enable=YES
        run_root sysrc rpc_lockd_enable=YES
        run_root sysrc rpc_statd_enable=YES

        run_root service nfsd onestart
        run_root service mountd onestart
        run_root service rpcbind onestart

        # Having root executing script from a user directory sounds dangerous
        # so let's install nfscron into libexec first.
        if is_force || [ ! -x "$pfnfsd" ]; then
            run_root mkdir -p "$(dirname $pfnfsd)"
            run_root cp "$BOOTSTRAP_ROOT/libexec/pfnfsd/pfnfsd.sh" $pfnfsd
            run_root chown root:wheel $pfnfsd
            run_root chmod 0700 $pfnfsd
        fi

        if is_force || [ ! -x "$nfscron" ]; then
            run_root mkdir -p "$(dirname $nfscron)"
            run_root cp "$BOOTSTRAP_ROOT/libexec/pfnfsd/periodic.sh" $nfscron
            run_root chown root:wheel $nfscron
            run_root chmod 0700 $nfscron
        fi

        cronline="@reboot $nfscron $netif"
        tmpcron=$BUILD_DIR/crontab.pfnfsd
        run_root crontab -u root -l > "$tmpcron" 2>/dev/null || true

        lineinfile \
            -f "$tmpcron" \
            -l "$cronline" \
            -r pfnfsd \
            -s present

        run_root crontab -u root - < "$tmpcron"
        run_root sh $nfscron "$netif"
        printe "pfnfsd crontab successfully installed"
    else
        printe "/etc/exports must already be configured"
    fi
}


## Runs
##

_run() {
    _setup_pf
}

_run_dev() {
    _setup_nfsd
}

run_with_flavors "$FLAVORS"
