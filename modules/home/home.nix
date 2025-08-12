{ config, pkgs, ... }:

{
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 7d";
  };

  nixGL.packages =
    if pkgs.stdenv.isLinux && !config.machine.isNixOS
    then pkgs.nixgl
    else null;
}
