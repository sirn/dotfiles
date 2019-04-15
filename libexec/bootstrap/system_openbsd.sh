#!/bin/sh -e
#
# Sets up OpenBSD system.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


## NFS
##

printe_h2 "Setting up nfsd..."

if [ ! -f /etc/exports ]; then
    run_root touch /etc/exports
fi

run_root rcctl enable portmap mountd nfsd
run_root rcctl start portmap mountd nfsd


## PF
##

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
    run_root cp "$root_dir/etc/pf/pf.openbsd.conf" /etc/pf.conf
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


## PF/NFSD
##

printe_h2 "Installing pfnfsd crontab..."

cronline="@reboot $root_dir/libexec/pfnfsd/periodic.sh vio0"
tmpcron=$build_dir/crontab.pfnfsd
run_root crontab -u root -l > "$tmpcron" 2>/dev/null || true

lineinfile \
    -f "$tmpcron" \
    -l "$cronline" \
    -r pfnfsd\\/periodic.sh \
    -s present

run_root crontab -u root - < "$tmpcron"
printe "pfnfsd crontab successfully installed"


## Tunings flavor
## See also https://dataswamp.org/~solene/2016-09-28-22.html
##

if has_args tunings "$flavors"; then
    printe_h2 "Tuning system..."

    if [ ! -f /etc/sysctl.conf ]; then
        run_root touch /etc/sysctl.conf
    fi

    for l in \
        kern.maxvnodes=768000 \
                      kern.maxfiles=32768 \
                      kern.maxclusters=256000 \
                      kern.seminfo.semmni=1024 \
                      kern.seminfo.semmns=4096 \
                      kern.shminfo.shmmax=805306368 \
                      kern.bufcachepercent=90
    do
        printe "${l}"

        key=${l%%=*}
        value=${l##*$key=}

        if [ "$(sysctl -n "$key")" != "$value" ]; then
            run_root sysctl "$l" >/dev/null
        fi

        lineinfile \
            -S \
            -f /etc/sysctl.conf \
            -r "$key=" \
            -l "$l"
    done

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
fi
