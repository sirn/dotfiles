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
    substituters = [ "https://cache.nixos.org/" ] ++ caches.substituters;
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ] ++ caches.trusted-public-keys;
  };
}
