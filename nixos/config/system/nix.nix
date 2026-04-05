{ pkgs, ... }:

{
  nix.channel.enable = false;

  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
}
