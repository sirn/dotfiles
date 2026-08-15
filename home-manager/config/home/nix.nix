{ pkgs, ... }:
let
  caches = import ../../../lib/caches.nix;
in
{
  nix.package = pkgs.nix;

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    substituters = caches.substituters;
    trusted-public-keys = caches.trusted-public-keys;
  };
}
