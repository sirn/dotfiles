{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.sessionVariablesExtra = ''
    export EDITOR=emacs
    export VISUAL=emacs
    export PATH=${dotfilesDir}/bin:${config.home.homeDirectory}/.local/bin:$PATH
  '' + (lib.optionalString isDarwin ''
    export PATH=/opt/local/sbin:/opt/local/bin:$PATH
  '');
}
