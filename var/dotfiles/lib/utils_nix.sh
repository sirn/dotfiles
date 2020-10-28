#!/bin/sh -e
#
# Shared functions for nix
#

NIX=/nix/var/nix/profiles/default/bin/nix
NIX_CHANNEL=/nix/var/nix/profiles/default/bin/nix-channel
NIX_ENV=/nix/var/nix/profiles/default/bin/nix-env
NIX_VERSION=2.3.8

nix_bootstrap() {
    if ! forced && [ -f $NIX ]; then
        return
    fi

    printe_h2 "Bootstrapping nix..."

    case "$(get_platform)" in
        darwin ) set -- --daemon --darwin-use-unencrypted-nix-store-volume;;
        * )      set -- --daemon;;
    esac

    fetch_url - https://releases.nixos.org/nix/nix-${NIX_VERSION}/install | sh -s - "$@"
}

nix_ensure_channel() {
    url=$1; shift
    name=${1:-nixpkgs}

    if ! $NIX_CHANNEL --list | grep -q "$url"; then
        printe_info "Adding nix channel..."

        "$NIX_CHANNEL" --add "$url" "$name"
        "$NIX_CHANNEL" --update -v
    else
        printe_info "Updating nix channel..."

        "$NIX_CHANNEL" --update -v
    fi
}

nix_install() {
    printe_info "Installing nix packages..."

    env NIXPKGS_CONFIG="$BASE_DIR/etc/nix/default.nix" $NIX_ENV -i all
}