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

if file_absent /etc/pf.conf.local; then
    run_root touch /etc/pf.conf.local
    run_root chown root:wheel /etc/pf.conf.local
    run_root chmod 0600 /etc/pf.conf.local
fi

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

if diff -u /etc/fstab /etc/fstab.new >/dev/null; then
    printf >&2 "/etc/fstab already updated\\n"
    rm /etc/fstab.new
    exit
fi

printf >&2 "/etc/fstab.new has been written!\\n"
printf >&2 "Please inspect the file and run: mv /etc/fstab.new /etc/fstab\\n"
printf >&2 "Not doing this automatically because it may render the system unbootable.\\n"
EOF
fi
