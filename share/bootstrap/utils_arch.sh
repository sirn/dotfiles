#!/bin/sh -e
#
# Shared functions for Arch Linux
#

PKGBUILD_ROOT=${XDG_DATA_HOME:-$HOME/.local/share}/pkgbuild
AUR_CHROOT=${AUR_CHROOT:-y}

aur_fetch() {
    pkg=$1; shift

    if command -v git >/dev/null; then
        git_clone \
            "https://aur.archlinux.org/$pkg.git" \
            "$PKGBUILD_ROOT/$pkg" \
            master
    else
        url=https://aur.archlinux.org/cgit/aur.git/snapshot/$pkg.tar.gz
        mkdir -p "$PKGBUILD_ROOT"
        fetch_url - "$url" | tar -C "$PKGBUILD_ROOT" -xzf -
    fi
}

aur_bootstrap() {
    if normalize_bool "$AUR_CHROOT"; then
        if command -v ccm64 >/dev/null; then
            return
        fi

        printe_h2 "Bootstrapping ccm..."

        case $(get_arch_variant) in
            artix-* )
                run_root pacman -S --asdeps artools rsync

                repo=\~sirn/aur-clean-chroot-manager-artix
                url=https://git.sr.ht/$repo/archive/master.tar.gz
                pkgdir=$PKGBUILD_ROOT/clean-chroot-manager-artix

                mkdir -p "$pkgdir"
                fetch_url - $url | tar -C "$pkgdir" -xzf - --strip-components=1
                cd "$pkgdir" || exit 1
                ;;

            * )
                run_root pacman -S --asdeps --noconfirm devtools rsync
                aur_fetch clean-chroot-manager
                cd "$PKGBUILD_ROOT/clean-chroot-manager" || exit 1
                ;;
        esac

        makepkg -Ci --noconfirm
        run_root ccm64
        run_root mkdir -p /scratch/.chroot64
    else
        if command -v yay >/dev/null; then
            return
        fi

        printe_h2 "Bootstrapping Yay (chroot unrequested)..."

        run_root pacman -S --noconfirm git
        run_root pacman -S --asdeps --noconfirm go
        aur_fetch yay
        cd "$PKGBUILD_ROOT/yay" || exit 1
        makepkg -Ci --noconfirm
    fi
}

aur_install() {
    pkg=$1; shift

    if pacman_installed "$pkg"; then
        printe "$pkg (pacman) already installed"
        return
    fi

    if normalize_bool "$AUR_CHROOT"; then
        printe_h2 "Installing $pkg (ccm)..."

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
    else
        printe_h2 "Installing $pkg (yay)..."
        yay -S --noconfirm "$pkg"
    fi
}

pacman_installed() {
    pkg=$1; shift
    if [ "$(pacman -Qq "$pkg" 2>&1)" != "$pkg" ]; then
        return 1
    fi
}

pacman_install() {
    pkg=$1; shift

    if pacman_installed "$pkg"; then
        printe "$pkg (pacman) already installed"
        return
    fi

    printe_h2 "Installing $pkg (pacman)..."
    run_root pacman -S --noconfirm "$pkg" "$@"
}

get_arch_variant() {
    variant=unknown

    if [ -f /etc/artix-release ] && pacman_installed runit; then
        variant=artix-runit
    elif [ -f /etc/artix-release ] && pacman_installed openrc; then
        variant=artix-openrc
    elif [ -f /etc/arch-release ]; then
        variant=arch
    fi

    echo "$variant"
}
