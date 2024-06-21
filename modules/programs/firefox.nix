{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  programs.firefox = {
    enable = isLinux;
  };
}
