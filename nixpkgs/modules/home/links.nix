{ config, lib, pkgs, ... }:

with lib;
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isLinux isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  home.file = mkMerge [
    {
      ".ideavimrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ideavim/ideavimrc"; };
    }
    (mkIf (isDarwin && config.machine.gui.enable) {
      ".amethyst.yml" = {
        source = mkOutOfStoreSymlink "${dotfilesDir}/etc/amethyst/amethyst.yml";
      };
    })
  ];
}
