{
  packageOverrides = pkgs: with pkgs; rec {
    localAria2 = import ./pkgs/aria2 {
      inherit stdenv fetchurl pkgconfig autoreconfHook cacert;
      inherit openssl c-ares libxml2 sqlite zlib libssh2;
    };

    localTrash = import ./pkgs/trash {
      inherit stdenv fetchurl clang;
      inherit (darwin.apple_sdk.frameworks) AppKit Cocoa ScriptingBridge;
    };

    localTmuxinator = import ./pkgs/tmuxinator {
      inherit stdenv bundlerEnv ruby;
    };

    python35LocalAutoflake = import ./pkgs/autoflake {
      inherit stdenv fetchurl python35Packages;
    };

    nodeLocalTypescript = import ./pkgs/typescript {
      inherit stdenv nodePackages;
    };

    nodeLocalTypescriptTools = import ./pkgs/typescript-tools {
      inherit stdenv nodePackages;
    };

    go16LocalSyncthing = import ./pkgs/syncthing {
      inherit stdenv fetchurl go_1_6;
    };

    all = buildEnv {
      name = "all";

      paths = [
        ansible2
        asciinema
        direnv
        emacs
        erlang
        ffmpeg
        fish
        ghc
        gitAndTools.gitFull
        gitAndTools.gitflow
        gitAndTools.hub
        go16LocalSyncthing
        haskellPackages.ShellCheck
        imagemagick
        ipcalc
        keychain
        leiningen
        localAria2
        localTmuxinator
        localTrash
        mercurial
        nodeLocalTypescript
        nodeLocalTypescriptTools
        nodejs
        openssh
        openssl
        python
        python35
        python35LocalAutoflake
        python35Packages.ipython
        python35Packages.pip
        python35Packages.virtualenv
        rebar3
        ruby
        rustc
        silver-searcher
        socat
        terraform
        tmux
        unbound
        xz
        youtube-dl
      ];

      passthru = {
        meta = {
          priority = 10;
        };
      };
    };
  };
}