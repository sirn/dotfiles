#!/bin/sh
# A simple script to setup pf pass rule for dynamically created network
# interfaces such as vboxnet using pf anchor. It is designed to be used
# with devd(8).
#
# For example, to automatically adding pass in rules for vboxnet devices
# once they're created, first add the following line to pf.conf:
#
#     anchor "pfpassif/*"
#
# Then create /usr/local/etc/devd/pfpassif_vboxnet.conf with:
#
#     notify 10 {
#         match "system"        "IFNET";
#         match "subsystem"     "vboxnet[0-9]+";
#         match "type"          "ATTACH";
#         action "/path/to/pfpassif.sh $subsystem up";
#     }
#
#     notify 10 {
#         match "system"        "IFNET";
#         match "subsystem"     "vboxnet[0-9]+";
#         match "type"          "DETACH";
#         action "/path/to/pfpassif.sh $subsystem down";
#     }
#
# Note that for some interface that rename itself after being created
# you may want to monitor LINK_UP and LINK_DOWN in devd(8) instead of
# ATTACH and DETACH, in which this script will get executed too early
# and it won't work.

_prefix="pfpassif"

hook_up() {
    (
        printf "icmp6_types=\"{ 128, 133, 134, 135, 136, 137 }\"\\n"
        printf "pass in on %s inet all\\n" "$1"
        printf "pass in on %s inet6 all\\n" "$1"
        printf "pass in on %s inet6 proto icmp6 all icmp6-type \$icmp6_types keep state\\n" "$1"
    ) | pfctl -a "$_prefix/$1" -f -
    logger -t "$_prefix" "Added pf anchor for $1."
}

hook_down() {
    pfctl -a "$_prefix/$1" -F rules
    logger -t "$_prefix" "Removed pf anchor for $1."
}

case "$2" in
    up)   hook_up   "$1";;
    down) hook_down "$1";;
    *)
        logger -t "$_prefix" "Oink. I don't know what to do with $2 on $1."
        exit 1
        ;;
esac
