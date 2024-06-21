{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  programs.thunderbird = {
    enable = isLinux;

    profiles = {
      main = {
        isDefault = true;
      };
    };
  };
}
