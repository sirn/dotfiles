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
      if isDarwin then
        pkgs.local.emacs-nox
      else
        if isLinux then
          pkgs.local.emacs-pgtk
        else
          pkgs.local.emacs-nox;

    extraPackages = epkgs: with epkgs; [
      # Early packages
      el-patch
      general
      no-littering
      org
      s
      use-package
      pkgs.local.emacsPackages.sqlite3

      # Packages
      ace-link
      ace-window
      avy
      consult
      consult-project-extra
      corfu
      corfu-prescient
      corfu-terminal
      ctrlf
      dired-sidebar
      embark
      embark-consult
      dtrt-indent
      editorconfig
      eldoc
      emacsql-sqlite
      envrc
      evil
      evil-collection
      evil-commentary
      evil-matchit
      evil-mc
      evil-surround
      flycheck
      flycheck-eglot
      flycheck-golangci-lint
      flycheck-posframe
      forge
      ghub
      git-commit
      git-gutter
      helpful
      magit
      marginalia
      modus-themes
      multi-vterm
      nix-ts-mode
      ob-restclient
      parinfer-rust-mode
      password-store
      pinentry
      posframe
      prescient
      project
      psc-ide
      rainbow-delimiters
      rainbow-mode
      restclient
      smartparens
      sql-indent
      tabspaces
      telephone-line
      tree-sitter
      treesit-grammars.with-all-grammars
      undo-tree
      unkillable-scratch
      vertico
      vertico-prescient
      visual-regexp
      vterm
      w3m
      which-key
      yasnippet
      yasnippet-snippets
      pkgs.local.emacsPackages.apheleia
      pkgs.local.emacsPackages.visual-regexp-steroids

      # Languages
      ansible
      ansible-doc
      clojure-mode
      dockerfile-mode
      elixir-mode
      emmet-mode
      erlang
      go-mode
      groovy-mode
      haskell-mode
      hcl-mode
      jq-mode
      json-mode
      jsonnet-mode
      lua-mode
      markdown-mode
      nim-mode
      nix-mode
      pandoc-mode
      php-mode
      purescript-mode
      rust-mode
      terraform-mode
      toml-mode
      typescript-mode
      web-mode
      yaml-mode

      # Non-Emacs dev packages (lsp and friends)
      pkgs.jq
      pkgs.nixpkgs-fmt
      pkgs.nodejs
      pkgs.pandoc
      pkgs.python311 # used by treemacs
      pkgs.ripgrep
      pkgs.shellcheck
      pkgs.shfmt
      pkgs.terraform
    ] ++ (if config.programs.notmuch.enable then [
      notmuch
    ] else [ ]) ++ (if isDarwin then [
      osx-trash
      pbcopy
    ] else [ ]);
  };

  home.file = {
    ".emacs.d/init.el" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/emacs/init.el";
    };
    ".emacs.d/var/parinfer-rust" = {
      source = "${pkgs.parinfer-rust}";
    };
    ".emacs.d/var/treesit-grammars" = {
      source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}";
    };
  };
}
