#!/bin/sh -e
#
# Sets up OpenBSD system.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root
ensure_platform "OpenBSD"

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

    for file in pf.conf.local pf.trusted; do
        if file_absent /etc/$file; then
            run_root touch /etc/$file
            run_root chown root:wheel /etc/$file
            run_root chmod 0600 /etc/$file
            printe "/etc/$file successfully created"
        fi
    done

    if is_force || [ ! -f /etc/pf.conf ]; then
        netif=$(_get_netif)
        sshd_port=$(_get_sshd_port)

        run_root cp "$BOOTSTRAP_ROOT/etc/pf/pf.openbsd.conf" /etc/pf.conf

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

        if ! run_root pfctl -nf /etc/pf.conf; then
            printe_err "\
/etc/pf.conf has been updated but contained errors, exiting\
"
            exit 1
        fi

        run_root pfctl -F all -f /etc/pf.conf
        printe "\
/etc/pf.conf has been updated, internet connection might be interrupted\
"
    else
        printe "/etc/pf.conf already exists, not overwriting"
    fi
}

_setup_ntpd() {
    printe_h2 "Configuring ntpd..."

    run_root touch /etc/rc.conf.local
    lineinfile \
        -S \
        -f /etc/rc.conf.local \
        -l "ntpd_flags=-s" \
        -r "^ntpd_flags=" \
        -s present

    run_root rcctl restart ntpd
}

_setup_nfsd() {
    printe_h2 "Installing pfnfsd crontab..."

    if [ -f /etc/exports ]; then
        printe_h2 "Setting up nfsd..."
        netif=$(_get_netif)

        pfnfsd=/usr/local/libexec/pfnfsd.sh
        nfscron=/usr/local/libexec/pfnfsd_periodic.sh

        run_root rcctl enable portmap mountd nfsd
        run_root rcctl start portmap mountd nfsd

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
    _setup_ntpd
}

_run_dev() {
    _setup_nfsd
}

_run_desktop() {
    printe_h2 "Configuring XenoDM..."
    run_root mkdir -p /etc/X11/xenodm
    run_root cp "$BOOTSTRAP_ROOT/etc/xenodm/Xsetup_0" /etc/X11/xenodm/Xsetup_0
    run_root chown root:wheel /etc/X11/xenodm/Xsetup_0
    run_root chmod 0755 /etc/X11/xenodm/Xsetup_0
    printe "/etc/X11/xenodm/Xsetup_0 has been updated"
}

run_with_flavors "$FLAVORS"
