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
      s
      use-package
      pkgs.local.emacsPackages.sqlite3

      # Org packages
      org
      org-modern
      org-ql
      org-super-agenda

      # Packages
      ace-link
      ace-window
      aidermacs
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
      embark
      embark-consult
      envrc
      evil
      evil-collection
      evil-commentary
      evil-matchit
      evil-mc
      evil-org
      evil-surround
      flycheck
      flycheck-eglot
      flycheck-golangci-lint
      flycheck-posframe
      flycheck-rust
      forge
      ghub
      git-gutter
      gptel
      helpful
      magit
      marginalia
      modus-themes
      multi-vterm
      nix-ts-mode
      ob-restclient
      orderless
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
      protobuf-mode
      purescript-mode
      rust-mode
      svelte-mode
      terraform-mode
      toml-mode
      typescript-mode
      web-mode
      with-editor
      yaml-mode
      zencoding-mode

    ] ++ (if config.programs.notmuch.enable then [
      # notmuch package also contains notmuch-mode so it needs to be here
      # instead of in emacs-bin-deps below
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
      source = pkgs.parinfer-rust-emacs;
    };
    ".emacs.d/var/treesit-grammars" = {
      source = pkgs.emacsPackages.treesit-grammars.with-all-grammars;
    };
    ".emacs.d/var/scowl" = {
      source = pkgs.scowl;
    };
    ".emacs.d/var/emacs-bin-deps" = {
      source = (pkgs.stdenv.mkDerivation {
        name = "emacs-bin-deps";
        buildInputs = with pkgs; [ makeWrapper ];
        nativeBuildInputs = with pkgs; [
          fd
          jq
          nixpkgs-fmt
          nodejs
          pandoc
          ripgrep
          shellcheck
          shfmt
          tenv
          hunspell.bin
          buf

          # LSPs
          gopls
          intelephense
          pyright
          nodePackages.svelte-language-server
          vscode-langservers-extracted
          typescript-language-server
          yaml-language-server
          protols

          # terraform-ls looks up `terraform` binary via $PATH but bin deps
          # is injected via exec-path only, so we need to inject bin deps path
          # explicitly here
          (terraform-ls.overrideDerivation (attrs: {
            nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/terraform-ls \
                --prefix PATH : ${config.home.homeDirectory}/.emacs.d/var/emacs-bin-deps
            '';
          }))
        ];

        phases = [ "installPhase" ];

        installPhase = ''
          mkdir -p "$out"

          for pkg in $nativeBuildInputs; do
            if [ -d "$pkg"/bin ]; then
              for bin in "$pkg/bin/"*; do
                if [ -x "$bin" ]; then
                  ln -s "$bin" "$out"/"$(basename "$bin")"
                fi
              done
            fi
          done
        '';
      });
    };
  };
}
