{ config, lib, pkgs, ... }:

with lib;
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isLinux isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  home.file = {
    ".config/foot/foot.ini" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/foot/foot.ini"; };
    ".config/fuzzel/fuzzel.ini" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/fuzzel/fuzzel.ini"; };
    ".config/sway/config" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/sway/config"; };
    ".config/waybar/config" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/waybar/config"; };
    ".config/waybar/style.css" = { source = mkOutOfStoreSymlink "${dotfilesDir}/etc/waybar/style.css"; };
  };

  home.sessionVariablesExtra = ''
    export XDG_CURRENT_DESKTOP=sway
    export QT_QPA_PLATFORMTHEME=kde
  '';
}
