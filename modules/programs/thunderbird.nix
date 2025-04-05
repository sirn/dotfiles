{ config, lib, pkgs, ... }:

{
  programs.thunderbird = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.thunderbird else null;

    profiles = {
      main = {
        isDefault = true;
      };
    };
  };
}
