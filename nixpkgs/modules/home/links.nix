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
      ".hgrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/hg/hgrc"; };
      ".ideavimrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ideavim/ideavimrc"; };
      ".kshrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ksh/kshrc"; };
      ".profile" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/sh/profile"; };
      ".ssh/config" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ssh/config"; };
    }
    (mkIf (isDarwin && config.machine.gui.enable) {
      ".amethyst.yml" = {
        source = mkOutOfStoreSymlink "${dotfilesDir}/etc/amethyst/amethyst.yml";
      };
    })
  ];
}
