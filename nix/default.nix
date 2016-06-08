{
  packageOverrides = pkgs: with pkgs; rec {
    callPackageDarwin = newScope darwin.apple_sdk.frameworks;

    erlangLocalRebar3 = callPackage ./pkgs/rebar3 { };
    go16LocalSyncthing = callPackage ./pkgs/syncthing { };
    localAria2 = callPackage ./pkgs/aria2 { };
    localGitFlow = callPackage ./pkgs/gitflow { };
    localTexlive = callPackage ./pkgs/texlive { };
    localTmuxinator = callPackage ./pkgs/tmuxinator { };
    localTrash = callPackageDarwin ./pkgs/trash { };
    nodeLocalTypescript = callPackage ./pkgs/typescript { };
    nodeLocalTypescriptTools = callPackage ./pkgs/typescript-tools { };
    python35LocalAutoflake = callPackage ./pkgs/autoflake { };

    all = buildEnv {
      name = "all";

      paths = [
        ansible2
        asciinema
        bundler
        direnv
        emacs
        erlang
        erlangLocalRebar3
        ffmpeg
        fish
        ghc
        gitAndTools.gitFull
        gitAndTools.hub
        go16LocalSyncthing
        haskellPackages.ShellCheck
        imagemagick
        ipcalc
        keychain
        leiningen
        localAria2
        localGitFlow
        localTexlive
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
