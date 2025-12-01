{ config, pkgs, ... }:

{
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 7d";
  };

  targets.genericLinux.nixGL.packages =
    if pkgs.stdenv.isLinux && config.targets.genericLinux.enable
    then pkgs.nixgl
    else null;
}
