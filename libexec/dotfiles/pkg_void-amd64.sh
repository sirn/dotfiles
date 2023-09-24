#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_void.sh"

_run() {
    xbps_install mosh
    xbps_install opendoas
    xbps_install rsync
    xbps_install snooze
    xbps_install xtools
}

_run_system() {
    xbps_install fuse3
    xbps_install iptables-nft
    xbps_alternative iptables iptables-nft
}

_run_sway() {
    xbps_install Waybar
    xbps_install clipman
    xbps_install foot
    xbps_install fuzzel
    xbps_install grim
    xbps_install libnotify
    xbps_install mako
    xbps_install qt5-wayland
    xbps_install qt6-wayland
    xbps_install slurp
    xbps_install sway
    xbps_install sway-audio-idle-inhibit
    xbps_install swaybg
    xbps_install swayidle
    xbps_install swaylock
}
