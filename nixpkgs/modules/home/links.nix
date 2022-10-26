{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  home.file = {
    ".hgrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/hg/hgrc"; };
    ".kshrc" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ksh/kshrc"; };
    ".profile" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/sh/profile"; };
    ".ssh/config" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/ssh/config"; };
    ".ssh/known_hosts" = { source = mkOutOfStoreSymlink "${dotprivDir}/etc/ssh/known_hosts"; };
  };
}
