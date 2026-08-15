{ pkgs, ... }:
let
  caches = import ../../../lib/caches.nix;
in
{
  nix.channel.enable = false;

  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  nix.settings = {
    substituters = caches.substituters;
    trusted-public-keys = caches.trusted-public-keys;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
}
