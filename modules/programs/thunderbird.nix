{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  programs.thunderbird = {
    enable = true;
    package = if isLinux then pkgs.thunderbird else null;

    profiles = {
      main = {
        isDefault = true;
      };
    };
  };
}
