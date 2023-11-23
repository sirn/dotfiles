{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  programs.password-store = {
    enable = true;
  };

  xdg.dataFile = {
    "password-store" = { source = mkOutOfStoreSymlink "${dotprivDir}/etc/password-store"; };
  };
}
