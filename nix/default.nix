{
  packageOverrides = pkgs: with pkgs; rec {
    callPackageDarwin = newScope darwin.apple_sdk.frameworks;

    # Horrible hack to make x509 works in OSX.
    go_1_6 = pkgs.go_1_6.overrideDerivation (oldAttrs: {
      patches = oldAttrs.patches ++ [
        ./pkgs/go-patch/x509_darwin_nonc.patch
      ];
    });

    erlangLocalRebar3 = callPackage ./pkgs/rebar3 { };
    go16LocalSyncthing = callPackage ./pkgs/syncthing { };
    localAria2 = callPackage ./pkgs/aria2 { };
    localGitFlow = callPackage ./pkgs/gitflow { };
    localTexlive = callPackage ./pkgs/texlive { };
    localTmuxinator = callPackage ./pkgs/tmuxinator { };
    localTrash = callPackageDarwin ./pkgs/trash { };
    nodeLocalTern = callPackage ./pkgs/tern { };
    nodeLocalTypescript = callPackage ./pkgs/typescript { };
    nodeLocalTypescriptTools = callPackage ./pkgs/typescript-tools { };
    python26LocalAnsible2 = callPackage ./pkgs/ansible2 { };
    python35LocalAutoflake = callPackage ./pkgs/autoflake { };

    all = buildEnv {
      name = "all";

      paths = [
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
        graphviz
        haskellPackages.ShellCheck
        imagemagick
        ipcalc
        irssi
        keychain
        leiningen
        localAria2
        localGitFlow
        localTexlive
        localTmuxinator
        localTrash
        mercurial
        nodeLocalTern
        nodeLocalTypescript
        nodeLocalTypescriptTools
        nodejs
        openssh
        openssl
        python
        python26LocalAnsible2
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
