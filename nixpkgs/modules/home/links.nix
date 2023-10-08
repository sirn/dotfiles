{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
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
