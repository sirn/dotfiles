#!/bin/sh -e
#
# Sets up OpenBSD system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

_setup_pf() {
    printe_h2 "Setting up pf..."

    if ! is_force && [ -f /etc/pf.conf ]; then
        printe_info "/etc/pf.conf already exists, not overwriting"
        return
    fi

    for file in pf.conf.local pf.trusted; do
        if file_absent /etc/$file; then
            run_root touch /etc/$file
            run_root chown root:wheel /etc/$file
            run_root chmod 0600 /etc/$file
            printe_info "/etc/$file successfully created"
        fi
    done

    netif=$(get_netif)
    sshd_port=$(get_sshd_port)

    run_root cp "$BASE_DIR/etc/pf/pf.openbsd.conf" /etc/pf.conf.new

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
    printe_h2 "Setting up nfsd..."

    if [ ! -f /etc/exports ]; then
        printe_info "/etc/exports is not configured, skipping..."
        return
    fi

    netif=$(get_netif)
    pfnfsd=/usr/local/libexec/pfnfsd.sh
    nfscron=/usr/local/libexec/pfnfsd_periodic.sh

    run_root rcctl enable portmap mountd nfsd
    run_root rcctl start portmap mountd nfsd

    printe_h2 "Installing pfnfsd crontab..."

    # Having root executing script from a user directory sounds dangerous
    # so let's install nfscron into libexec first.
    if is_force || [ ! -x "$pfnfsd" ]; then
        run_root mkdir -p "$(dirname $pfnfsd)"
        run_root cp "$BASE_DIR/libexec/pfnfsd/pfnfsd.sh" $pfnfsd
        run_root chown root:wheel $pfnfsd
        run_root chmod 0700 $pfnfsd
    fi

    if is_force || [ ! -x "$nfscron" ]; then
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


## Runs
##

_run() {
    _setup_pf
    _setup_ntpd
}

_run_desktop() {
    printe_h2 "Configuring XenoDM..."

    run_root mkdir -p /etc/X11/xenodm
    run_root cp "$BASE_DIR/etc/xenodm/Xsetup_0" /etc/X11/xenodm/Xsetup_0
    run_root chown root:wheel /etc/X11/xenodm/Xsetup_0
    run_root chmod 0755 /etc/X11/xenodm/Xsetup_0

    printe_info "/etc/X11/xenodm/Xsetup_0 has been updated"
}

_run_dev() {
    _setup_nfsd
}

run_with_flavors "$@"
