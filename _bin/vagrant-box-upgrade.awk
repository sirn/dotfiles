#!/usr/bin/awk -f
#
# This is a simple script to upgrade all Vagrant boxes.
#
# This script is written in AWK and expected to be run on a system
# which AWK is installed as /usr/bin/awk since `env -S awk -f` to be
# able to pass flags to awk via env is not POSIX complaint.
#

BEGIN {
    if (system("hash vagrant >/dev/null 2>&1")) {
        print "Vagrant is required to be installed."
        exit 1
    }

    while ("vagrant box outdated --global" | getline line) {
        if (line ~ /is outdated/) {
            split(line, a)
            box = substr(a[2], 2, length(a[2])-2);
            provider = substr(a[4], 2, length(a[4])-2);
            system("vagrant box add -f -c --provider=" provider " " box)
        }
    }

    system("vagrant box prune -f")
}
