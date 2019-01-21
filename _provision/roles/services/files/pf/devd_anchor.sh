#!/bin/sh
#
# A devd hook to automatically setup pf firewall for dynamically created
# network interfaces such as vboxnet or zerotier. To use this script
# create /usr/local/etc/devd/pf_ifnet.conf with:
#
#     notify 10 {
#         match "system"        "IFNET";
#         match "subsystem"     "vboxnet[0-9]+";
#         match "type"          "ATTACH";
#         action "/usr/local/libexec/local-pf-ifinet $subsystem up";
#     }
#
#     notify 10 {
#         match "system"        "IFNET";
#         match "subsystem"     "vboxnet[0-9]+";
#         match "type"          "DETACH";
#         action "/usr/local/libexec/local-pf-ifinet $subsystem down";
#     }
#
# Then configure pf.conf with the following line:
#
#     anchor "pfifnet/*"
#
# Then put this script on the path given in action. Note that on some
# tap interface such as zerotier, you may want to monitor LINK_UP
# and LINK_DOWN instead.
#

_tag="pf_ifnet"
_prefix="pfifnet"

hook_up() {
    (
        echo "pass in on $1 inet all"
        echo "pass in on $1 inet6 all"
    ) | pfctl -a "$_prefix/$1" -f -
    logger -t "$_tag" "Added pf anchor for $1."
}

hook_down() {
    pfctl -a "$_prefix/$1" -F rules
    logger -t "$_tag" "Removed pf anchor for $1."
}

case "$2" in
    up)   hook_up   "$1";;
    down) hook_down "$1";;
    *)
        logger -t "$_tag" "Oink. I don't know what to do with $2 on $1."
        exit 1
        ;;
esac
