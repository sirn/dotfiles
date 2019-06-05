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


## Getting primary network interface
##

NETIF=$(ifconfig | awk '! /^lo|^pf|^enc|^\t/ { FS=":"; print $1; exit }')

if ! ifconfig "$NETIF" >/dev/null 2>&1; then
    printe_err "Could not determine primary network interface"
    exit 1
fi


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
        run_root cp "$BOOTSTRAP_ROOT/etc/pf/pf.openbsd.conf" /etc/pf.conf
        lineinfile -S \
                   -f /etc/pf.conf \
                   -l "ext_if=$NETIF" \
                   -r "^ext_if=" \
                   -s present

        run_root chown root:wheel /etc/pf.conf
        run_root chmod 0600 /etc/pf.conf

        if ! run_root pfctl -nf /etc/pf.conf; then
            printe_err "/etc/pf.conf has been updated but contained errors, exiting"
            exit 1
        fi

        run_root pfctl -F all -f /etc/pf.conf
        printe "/etc/pf.conf has been updated, internet connection might be interrupted"
    else
        printe "/etc/pf.conf already exists, not overwriting"
    fi
}

_setup_ntpd() {
    printe_h2 "Configuring ntpd..."

    lineinfile -S \
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

        run_root rcctl enable portmap mountd nfsd
        run_root rcctl start portmap mountd nfsd

        cronline="@reboot $BOOTSTRAP_ROOT/libexec/pfnfsd/periodic.sh $NETIF"
        tmpcron=$BUILD_DIR/crontab.pfnfsd
        run_root crontab -u root -l > "$tmpcron" 2>/dev/null || true

        lineinfile \
            -f "$tmpcron" \
            -l "$cronline" \
            -r pfnfsd\\/periodic.sh \
            -s present

        run_root crontab -u root - < "$tmpcron"
        printe "pfnfsd crontab successfully installed"
    else
        printe "/etc/exports must already be configured"
    fi
}


## Desktop flavor
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

_run_tunings() {
    ## See also https://dataswamp.org/~solene/2016-09-28-22.html
    printe_h2 "Tuning filesystem..."

    run_root sh <<EOF
awk '
s = /ffs rw/ {
  if (\$4 !~ /noatime/) {
    if (\$4 == "rw") { sub("rw", "rw,noatime") }
    else { sub("rw,", "rw,noatime,") }
  }
  if (\$4 !~ /softdep/) {
    if (\$4 == "rw") { sub("rw", "rw,softdep") }
    else { sub("rw,", "rw,softdep,") }
  }
  print
} ! s { print }
' < /etc/fstab > /etc/fstab.new
EOF

    if run_root diff -u /etc/fstab /etc/fstab.new >/dev/null; then
        printe "/etc/fstab already updated"
        run_root rm /etc/fstab.new
        exit
    fi

    printe "/etc/fstab.new has been written!"
    printe "Please inspect the file and run: mv /etc/fstab.new /etc/fstab"
    printe "Not doing this automatically because it may render the system unbootable."
}

run_with_flavors "$FLAVORS"
