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
    package =
      if config.machine.gui.enable && isLinux then
        pkgs.local.emacsNativeComp-pgtk
      else
        pkgs.local.emacsNativeComp-nox;

    extraPackages = epkgs: with pkgs; [
      epkgs.w3m
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
      local.emacsPackages.sqlite3

      # dev packages (non-lsp)
      nodejs
      pandoc
      shellcheck
      shfmt
      terraform

      # lsps
      rnix-lsp
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
