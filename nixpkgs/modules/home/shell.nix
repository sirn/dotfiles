{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  homeDir = config.home.homeDirectory;
  dotfilesDir = "${homeDir}/.dotfiles";
  dotprivDir = "${homeDir}/.dotfiles";
in
{
  home.sessionVariablesExtra = ''
    export EDITOR=emacs
    export VISUAL=emacs
    export PATH=${dotfilesDir}/bin:${homeDir}/.local/bin:$PATH
    export GOPATH=${homeDir}/Dev/go/gopath:${homeDir}/Dev
  '' + (lib.optionalString isDarwin ''
    export PATH=/opt/local/sbin:/opt/local/bin:$PATH
  '');
}
