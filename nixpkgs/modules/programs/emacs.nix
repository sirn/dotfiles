{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home) homeDirectory;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.local.emacsNativeComp-nox;

    extraPackages = epkgs: [
      epkgs.w3m
      epkgs.vterm
      pkgs.local.emacsPackages.sqlite3
    ] ++ (if config.programs.notmuch.enable then [
      epkgs.notmuch
    ] else [ ]);
  };

  home.file = {
    ".emacs.d/init.el" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/emacs/init.el";
    };
    ".emacs.d/straight/versions/default.el" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/emacs/straight/versions/default.el";
    };
    ".emacs.d/var/parinfer-rust" = {
      source = "${pkgs.parinfer-rust}/lib";
    };
  };
}
