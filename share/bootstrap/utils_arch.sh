#!/bin/sh -e
#
# Shared functions for Arch Linux
#

PKGBUILD_ROOT=$HOME/.data/pkgbuild

aur_fetch() {
    pkg=$1; shift
    url=https://aur.archlinux.org/cgit/aur.git/snapshot/$pkg.tar.gz
    mkdir -p "$PKGBUILD_ROOT"
    fetch_url - "$url" | tar -C "$PKGBUILD_ROOT" -xzf -
}

ccm_bootstrap() {
    if command -v ccm64 >/dev/null; then
        return
    fi

    printe_h2 "Bootstrapping ccm..."

    if [ -f /etc/artix-release ]; then
        repo=\~sirn/aur-clean-chroot-manager-artix
        url=https://git.sr.ht/$repo/archive/master.tar.gz
        pkgdir=$PKGBUILD_ROOT/clean-chroot-manager-artix

        mkdir -p "$pkgdir"
        fetch_url - $url | tar -C "$pkgdir" -xzf - --strip-components=1
        cd "$pkgdir" || exit 1
    else
        aur_fetch clean-chroot-manager
        cd "$PKGBUILD_ROOT/clean-chroot-manager" || exit 1
    fi

    makepkg -Ci --noconfirm
}

ccm_install() {
    pkg=$1; shift

    if pacman_installed "$pkg"; then
        printe "$pkg (pacman) already installed"
        return
    fi

    printe_h2 "Installing $pkg (pacman via ccm)..."

    for n in "$PKGBUILD_ROOT/$pkg/"*".pkg.tar.xz"; do
        if [ -f "$n" ]; then
            printe_info "$n found and may be outdated, removing..."
            rm "$n"
        fi
    done

    aur_fetch "$pkg"
    cd "$PKGBUILD_ROOT/$pkg"

    run_root ccm64 s
    run_root pacman -U --noconfirm "$PKGBUILD_ROOT/$pkg/"*".pkg.tar.xz"
}

pacman_installed() {
    pkg=$1; shift
    pacman -Qq "$pkg" >/dev/null 2>&1
}

pacman_install() {
    pkg=$1; shift

    if pacman_installed "$pkg"; then
        printe "$pkg (pacman) already installed"
        return
    fi

    printe_h2 "Installing $pkg (pacman)..."
    run_root pacman -S --noconfirm "$pkg"
}
