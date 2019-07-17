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

_setup_nfsd() {
    printe_h2 "Setting up nfsd..."

    if [ ! -f /etc/exports ]; then
        printe_info "/etc/exports is not configured, skipping..."
        return
    fi

    netif=$(get_netif)
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

    printe_h2 "Installing pfnfsd crontab..."

    # Having root executing script from a user directory sounds dangerous
    # so let's install nfscron into libexec first.
    if forced || [ ! -f "$pfnfsd" ]; then
        run_root mkdir -p "$(dirname $pfnfsd)"
        run_root cp "$BASE_DIR/libexec/pfnfsd/pfnfsd.sh" $pfnfsd
        run_root chown root:wheel $pfnfsd
        run_root chmod 0700 $pfnfsd
    fi

    if forced || [ ! -f "$nfscron" ]; then
        run_root mkdir -p "$(dirname $nfscron)"
        run_root cp "$BASE_DIR/libexec/pfnfsd/periodic.sh" $nfscron
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
    printe_info "pfnfsd crontab successfully installed"
}

_run() {
    _setup_pf
}

_run_dev() {
    _setup_nfsd
}

run_with_flavors "$@"
