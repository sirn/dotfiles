{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    all = buildEnv {
      name = "all";
      paths = [
        glibcLocales
        dev
      ];

      passthru = {
        meta = {
          priority = 10;
        };
      };
    };

    dev = buildEnv {
      name = "dev";
      paths = [
        entr
        graphviz
        jq
        jsonnet
        pandoc
        postgresql_12
        proselint
        rnix-lsp
        shellcheck

        # Groups
        elixirDev
        erlangDev
        gitDev
        goDev
        kubernetesDev
        nodeDev
        opsDev
        python3Dev
        rubyDev
        tclDev
      ];
    };

    elixir-ls = callPackage ./pkgs/elixir-ls { };

    elixirDev = buildEnv {
      name = "elixirDev";
      paths = [
        elixir
        elixir-ls
      ];
    };

    erlang-ls = callPackage ./pkgs/erlang-ls { };

    erlangDev = buildEnv {
      name = "erlangDev";
      paths = [
        erlang
        rebar3
        erlang-ls
      ];
    };

    gitDev = buildEnv {
      name = "gitDev";
      paths = [
        git
        git-crypt
        git-lfs
        gitAndTools.pre-commit
      ];
    };

    # Probably better use lowPrio, but I will have to move gotools
    # up to dev, or make the whole goDev group lowPrio which is
    # not what I wanted (due to how Nix handle priority).
    gotools = pkgs.gotools.overrideAttrs (oldAttrs: rec {
      postInstall = oldAttrs.postInstall + ''
        rm $out/bin/bundle
        rm $out/bin/doc
      '';
    });

    goDev = buildEnv {
      name = "goDev";
      paths = [
        go
        gocode
        gotools
        golangci-lint
        golint
        gopls
      ];
    };

    kapitan = callPackage ./pkgs/kapitan { };

    kubernetesDev = buildEnv {
      name = "kubernetesDev";
      paths = [
        kapitan
        kubectl
        kubectx
        kubernetes-helm
        kustomize
      ];
    };

    localNodePackages = callPackage ./pkgs/node-packages { };

    nodeDev = buildEnv {
      name = "nodeDev";
      paths = [
        localNodePackages.npm-upgrade
        localNodePackages.stylelint
        localNodePackages.stylelint-config-recommended
        localNodePackages.stylelint-config-recommended-scss
        localNodePackages.stylelint-scss
        nodePackages.eslint
        nodePackages.npm
        nodePackages.prettier
        nodePackages.tern
        nodePackages.typescript
        nodePackages.typescript-language-server
        nodePackages.yaml-language-server
        nodejs
      ];
    };

    opsDev = buildEnv {
      name = "opsDev";
      paths = [
        ansible
        cloudflared
        google-cloud-sdk
        ipcalc
        linode-cli
        nomad
        terraform-lsp
        terraform_0_13
      ];
    };

    python3Pyls = callPackage ./pkgs/python-pyls { };

    python3Dev = buildEnv {
      name = "python3Dev";
      paths = [
        python38Full
        python38Packages.pip
        python38Packages.poetry
        python38Packages.black
        python38Packages.flake8
        python3Pyls.pyls-black
        python3Pyls.pyls-isort
        python3Pyls.pyls-mypy
        python3Pyls.python-language-server
      ];
    };

    rubyDev = buildEnv {
      name = "rubyDev";
      paths = [
        ruby
      ];
    };

    tclDev = buildEnv {
      name = "tclDev";
      paths = [
        tcl
      ];
    };
  };
}
