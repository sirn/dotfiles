#!/bin/sh -e
#
# Shared functions for Ubuntu Linux
#

apt_installed() {
    pkg=$1; shift
    dpkg --get-selections |
        awk '$2 == "install" { print $1 }' |
        grep -q ^"$pkg"$
}

apt_bootstrap() {
    if ! apt_installed software-properties-common; then
        run_root apt-get install -y software-properties-common
    fi
}

apt_setup_ppa() {
    ppa=$1; shift
    ppa=${ppa#ppa:}
    ppa_url=https?://ppa.launchpad.net/$ppa/ubuntu

    if apt-cache policy | grep -Eq "$ppa_url"; then
        printe "$ppa (ppa) already added"
        return
    fi

    run_root apt-add-repository -y "ppa:$ppa"
}

apt_install() {
    pkg=$1; shift

    if apt_installed "$pkg"; then
        printe "$pkg (apt) already installed"
        return
    fi

    printe "Installing $pkg (apt)..."
    run_root apt-get install -y "$pkg"
}

snap_installed() {
    pkg=$1; shift
    snap list "$pkg" >/dev/null 2>&1
}

snap_install() {
    pkg=$1; shift

    if snap_installed "$pkg"; then
        printe "$pkg (snap) already installed"
        return
    fi

    printe "Installing $pkg (snap)..."
    run_root snap install "$pkg" "$@"
}
