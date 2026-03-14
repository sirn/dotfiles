{ config, pkgs, ... }:

{
  nix.gc = {
    automatic = true;
  };

  targets.genericLinux.nixGL.packages =
    if pkgs.stdenv.isLinux && config.targets.genericLinux.enable then pkgs.nixgl else null;
}
