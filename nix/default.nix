{
  packageOverrides = pkgs: with pkgs; rec {
    callPackageDarwin = newScope darwin.apple_sdk.frameworks;

    localAria2 = callPackage ./pkgs/aria2 { };
    localTrash = callPackageDarwin ./pkgs/trash { };
    localTmuxinator = callPackage ./pkgs/tmuxinator { };
    python35LocalAutoflake = callPackage ./pkgs/autoflake { };
    nodeLocalTypescript = callPackage ./pkgs/typescript { };
    nodeLocalTypescriptTools = callPackage ./pkgs/typescript-tools { };
    go16LocalSyncthing = callPackage ./pkgs/syncthing { };

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