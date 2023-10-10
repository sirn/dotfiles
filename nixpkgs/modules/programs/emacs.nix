{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux isDarwin;
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
      # Early packages
      epkgs.el-patch
      epkgs.general
      epkgs.no-littering
      epkgs.use-package
      epkgs.org
      local.emacsPackages.sqlite3

      # Packages
      epkgs.ace-link
      epkgs.apheleia
      epkgs.avy
      epkgs.consult
      epkgs.consult-project-extra
      epkgs.corfu
      epkgs.corfu-prescient
      epkgs.corfu-terminal
      epkgs.ctrlf
      epkgs.dtrt-indent
      epkgs.editorconfig
      epkgs.eldoc
      epkgs.emacsql-sqlite
      epkgs.envrc
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-commentary
      epkgs.evil-matchit
      epkgs.evil-mc
      epkgs.evil-surround
      epkgs.flycheck
      epkgs.flycheck-golangci-lint
      epkgs.forge
      epkgs.ghub
      epkgs.git-commit
      epkgs.git-gutter
      epkgs.helpful
      epkgs.lsp-ui
      epkgs.magit
      epkgs.marginalia
      epkgs.modus-themes
      epkgs.multi-vterm
      epkgs.ob-restclient
      epkgs.parinfer-rust-mode
      epkgs.password-store
      epkgs.pinentry
      epkgs.prescient
      epkgs.project
      epkgs.psc-ide
      epkgs.rainbow-delimiters
      epkgs.rainbow-mode
      epkgs.restclient
      epkgs.smartparens
      epkgs.sql-indent
      epkgs.telephone-line
      epkgs.tree-sitter
      epkgs.treesit-grammars.with-all-grammars
      epkgs.undo-tree
      epkgs.unkillable-scratch
      epkgs.vertico
      epkgs.vertico-prescient
      epkgs.visual-regexp
      epkgs.visual-regexp-steroids
      epkgs.vterm
      epkgs.w3m
      epkgs.which-key
      epkgs.winum
      epkgs.yasnippet
      epkgs.yasnippet-snippets

      # Languages
      epkgs.ansible
      epkgs.ansible-doc
      epkgs.clojure-mode
      epkgs.dockerfile-mode
      epkgs.elixir-mode
      epkgs.emmet-mode
      epkgs.erlang
      epkgs.go-mode
      epkgs.groovy-mode
      epkgs.haskell-mode
      epkgs.hcl-mode
      epkgs.jq-mode
      epkgs.json-mode
      epkgs.jsonnet-mode
      epkgs.lsp-mode
      epkgs.lua-mode
      epkgs.markdown-mode
      epkgs.nim-mode
      epkgs.nix-mode
      epkgs.pandoc-mode
      epkgs.php-mode
      epkgs.purescript-mode
      epkgs.rust-mode
      epkgs.terraform-mode
      epkgs.toml-mode
      epkgs.typescript-mode
      epkgs.web-mode
      epkgs.yaml-mode

      # Non-Emacs dev packages (lsp and friends)
      jq
      nodejs
      pandoc
      ripgrep
      rnix-lsp
      shellcheck
      shfmt
      terraform
    ] ++ (if config.programs.notmuch.enable then [
      epkgs.notmuch
    ] else [ ]) ++ (if isDarwin then [
      epkgs.osx-trash
      epkgs.pbcopy
    ] else [ ]);
  };

  home.file = {
    ".emacs.d/init.el" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/emacs/init.el";
    };
    ".emacs.d/var/parinfer-rust" = {
      source = "${pkgs.parinfer-rust}";
    };
  };
}
