{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.emacs = {
    enable = true;
    package =
      lib.mkDefault
        (if pkgs.stdenv.isLinux then
          pkgs.emacs-pgtk
        else
          pkgs.emacs-nox);

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
      corfu
      corfu-prescient
      corfu-terminal
      ctrlf
      dired-sidebar
      dtrt-indent
      editorconfig
      eldoc
      emacsql-sqlite
      embark
      embark-consult
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
      flycheck-rust
      forge
      ghub
      git-gutter
      helpful
      magit
      marginalia
      modus-themes
      multi-vterm
      nix-ts-mode
      ob-restclient
      parinfer-rust-mode
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

      (apheleia.overrideDerivation
        (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

          postInstall = ''
            wrapProgram $out/share/emacs/site-lisp/elpa/${attrs.pname}-${attrs.version}/scripts/formatters/apheleia-npx \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nodePackages.prettier ]}
            wrapProgram $out/share/emacs/site-lisp/elpa/${attrs.pname}-${attrs.version}/scripts/formatters/apheleia-phpcs \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.php83Packages.php-codesniffer ]}
          '';
        }))

      (visual-regexp-steroids.overrideDerivation (attrs: {
        postPatch = ''
          substituteInPlace visual-regexp-steroids.el \
            --replace "python %s" "${pkgs.python311}/bin/python3 %s"
        '';
      }))

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
      with-editor
      yaml-mode

      # Non-Emacs dev packages (lsp and friends)
      pkgs.fd
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
    ] else [ ]) ++ (if pkgs.stdenv.isDarwin then [
      osx-trash
      pbcopy
    ] else [ ]);
  };

  home.file = {
    ".emacs.d/init.el" = {
      source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/etc/emacs/init.el";
    };
    ".emacs.d/var/parinfer-rust" = {
      source = "${pkgs.parinfer-rust-emacs}";
    };
    ".emacs.d/var/treesit-grammars" = {
      source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}";
    };
  };
}
