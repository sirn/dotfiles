{ config, lib, pkgs, ... }:

let
  cfg = config.programs.emacs;

  notmuchcfg = config.programs.notmuch;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  # Wrap tenv to auto-install appropriate terraform version
  tenvWrapped = pkgs.tenv.overrideDerivation (attrs: {
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

    postInstall = ''
      for program in "$out"/bin/*; do
        if [ -f "$program" ]; then
          wrapProgram "$program" --set TENV_AUTO_INSTALL true
        fi
      done
    '';
  });

  hasGui = swaycfg.enable || niricfg.enable;

  baseEmacs =
    if (pkgs.stdenv.isLinux && hasGui) then pkgs.emacs-pgtk
    else if pkgs.stdenv.isDarwin then pkgs.emacs
    else pkgs.emacs-nox;

  emacsBinDeps = pkgs.stdenv.mkDerivation {
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
      hunspell.bin
      buf

      # LSPs
      clang-tools
      clojure-lsp
      gopls
      intelephense
      nil
      pyright
      rubyPackages.ruby-lsp
      nodePackages.bash-language-server
      dockerfile-language-server
      nodePackages.svelte-language-server
      vscode-langservers-extracted
      typescript-language-server
      yaml-language-server
      protols

      tenvWrapped

      # terraform-ls looks up `terraform` binary via $PATH but bin deps
      # is injected via exec-path only, so we need to inject bin deps path
      # explicitly here
      (terraform-ls.overrideDerivation (attrs: {
        nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
        postInstall = ''
          wrapProgram $out/bin/terraform-ls \
            --prefix PATH : ${tenvWrapped}/bin
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
  };

  earlyInitEl = pkgs.writeText "early-init.el" ''
    (setq inhibit-startup-screen t)
    (defvar gemacs-nix-config-directory user-emacs-directory)
    (defvar gemacs-default-shell "${config.machine.interactiveShell}")

    ;; Redirect writable state to ~/.emacs.d/ since user-emacs-directory
    ;; points to the read-only Nix store
    (defvar no-littering-etc-directory (expand-file-name "etc/" "~/.emacs.d/"))
    (defvar no-littering-var-directory (expand-file-name "var/" "~/.emacs.d/"))
  '';

  emacsConfigDir = pkgs.runCommand "emacs-config" { } ''
    mkdir -p $out/{packages,var}

    cp ${earlyInitEl} $out/early-init.el
    cp ${../../etc/emacs/init.el} $out/init.el

    for f in ${../../etc/emacs/packages}/*.el; do
      cp "$f" $out/packages/
    done

    ln -s ${pkgs.parinfer-rust-emacs} $out/var/parinfer-rust
    ln -s ${(pkgs.emacsPackagesFor baseEmacs).treesit-grammars.with-all-grammars} $out/var/treesit-grammars
    ln -s ${pkgs.scowl} $out/var/scowl
    ln -s ${emacsBinDeps} $out/var/emacs-bin-deps

    cp -r ${../../etc/emacs/templates} $out/var/templates
  '';

  wrappedEmacs = pkgs.symlinkJoin {
    name = "emacs-wrapped";
    paths = [ baseEmacs ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --add-flags "--init-directory=${emacsConfigDir}"
    '';
    inherit (baseEmacs) meta;
    passthru = (baseEmacs.passthru or { }) // {
      inherit (baseEmacs) src;
    } // lib.optionalAttrs (baseEmacs ? LIBRARY_PATH) {
      inherit (baseEmacs) LIBRARY_PATH;
    };
  };

in
{
  programs.emacs = {
    enable = true;

    package = lib.mkDefault wrappedEmacs;

    extraPackages = epkgs: with epkgs; [
      # Early packages
      el-patch
      general
      no-littering
      pkgs.local.emacsPackages.sqlite3
      s
      use-package

      # Org packages
      org
      org-modern
      org-ql
      org-super-agenda

      # Packages
      ace-link
      ace-window
      avy
      clipetty
      consult
      corfu
      corfu-prescient
      corfu-terminal
      doom-modeline
      dtrt-indent
      eat
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
      flymake-collection
      forge
      ghub
      git-gutter
      gptel
      helpful
      magit
      marginalia
      modus-themes
      nerd-icons
      nerd-icons-dired
      nix-ts-mode
      ob-restclient
      orderless
      outline-indent
      parinfer-rust-mode
      pinentry
      prescient
      project
      psc-ide
      rainbow-delimiters
      rainbow-mode
      restclient
      smartparens
      sql-indent
      tempel
      treemacs
      treemacs-evil
      treemacs-nerd-icons
      treesit-grammars.with-all-grammars
      vundo
      unkillable-scratch
      vertico
      vertico-prescient
      visual-regexp
      vterm
      which-key

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
      clojure-ts-mode
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

    ] ++ (if notmuchcfg.enable then [
      # notmuch package also contains notmuch-mode so it needs to be here
      # instead of in emacs-bin-deps below
      notmuch
    ] else [ ]) ++ (if pkgs.stdenv.isDarwin then [
      exec-path-from-shell
      osx-trash
      pbcopy
    ] else [ ]);
  };
}
