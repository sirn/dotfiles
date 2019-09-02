#!/bin/sh -e
#
# Shared functions for Ubuntu Linux
#

apt_installed() {
    pkg=$1; shift
    dpkg --get-selections |
        awk '$2 == "install" { split($1, s, ":"); print s[1] }' |
        grep -q ^"$pkg"$
}

apt_bootstrap() {
    for pkg in aptitude software-properties-common; do
        if ! apt_installed $pkg; then
            env DEBIAN_FRONTEND=noninteractive apt-get install -y $pkg
        fi
    done
}

apt_setup_repo() {
    key=$1; shift
    uri=$1; shift
    suite=$1; shift

    suite=$(echo "$suite" | sed "s|LSB_RELEASE|$(lsb_release -sc)|")

    if apt-cache policy | grep -q "$uri $suite"; then
        printe "$uri (repo) already added"
        return
    fi

    if [ -n "$key" ] && [ "$key" != "NO_KEY" ]; then
        keyname=$(basename "$key")
        fetch_url "$keyname.asc" "$key"
        run_root apt-key add "$keyname.asc"
        rm "$keyname.asc"
    fi

    run_root apt-add-repository -y "deb $uri $suite $@"
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

apt_select() {
    name=$1; shift
    link=$1; shift
    target=$1; shift
    priority=$1

    if [ -z "$priority" ]; then
        priority=10
    fi

    run_root update-alternatives --install "$link" "$name" "$target" "$priority"
}

apt_install() {
    pkg=$1; shift

    if apt_installed "$pkg"; then
        printe "$pkg (apt) already installed"
        return
    fi

    printe "Installing $pkg (apt)..."
    run_root env DEBIAN_FRONTEND=noninteractive aptitude install -y "$pkg"
}
