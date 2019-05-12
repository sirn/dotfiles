#!/bin/sh -e
#
# Sets up FreeBSD system.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Getting primary network interface
##

netif=$(ifconfig | awk '! /^lo|^pf|^enc|^\t/ { FS=":"; print $1; exit }')

if ! ifconfig "$netif" >/dev/null 2>&1; then
    printe_err "Could not determine primary network interface"
    exit 1
fi


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


## PF
##

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
    run_root cp "$root_dir/etc/pf/pf.freebsd.conf" /etc/pf.conf
    lineinfile -S \
        -f /etc/pf.conf \
        -l "ext_if=$netif" \
        -r "^ext_if=" \
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


## NFS
##

printe_h2 "Setting up nfsd..."

if [ -f /etc/exports ]; then
    run_root sysrc nfs_server_enable=YES
    run_root sysrc mountd_enable=YES
    run_root sysrc rpcbind_enable=YES
    run_root sysrc rpc_lockd_enable=YES
    run_root sysrc rpc_statd_enable=YES

    run_root service nfsd onestart
    run_root service mountd onestart
    run_root service rpcbind onestart

    cronline="@reboot $root_dir/libexec/pfnfsd/periodic.sh $netif"
    tmpcron=$build_dir/crontab.pfnfsd
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
