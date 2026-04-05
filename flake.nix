{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.11";
    };

    nixpkgs-unstable = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Nix quality of life
    ##

    # So we don't have to reindex nix-locate by ourselves.
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Storing secrets using SOPS.
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # For GL application compatibility on non-Nix.
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Multi-file formatting with Nix
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Package overlays
    ##

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      config = {
        allowUnfree = true;
      };

      compatOverlays = [
        (final: prev: {
          # yt-dlp includes secretstorage (for Gnome keyring) which depends on jeepney.
          # jeepney tests fail on Darwin due to missing D-Bus session bus.
          # secretstorage is only needed for --cookies-from-browser on Linux.
          # Remove once https://github.com/NixOS/nixpkgs/issues/493775 is in unstable.
          yt-dlp =
            if prev.stdenv.hostPlatform.isDarwin then
              prev.yt-dlp.overridePythonAttrs (oldAttrs: {
                dependencies = prev.lib.filter (
                  p:
                  !(prev.lib.elem (p.pname or "") [
                    "cffi"
                    "secretstorage"
                  ])
                ) oldAttrs.dependencies;
              })
            else
              prev.yt-dlp;
        })

        (final: prev: {
          # inetutils 2.7 has a format string bug that fails with strict compiler flags
          # See: https://github.com/NixOS/nixpkgs/issues/488689
          inetutils =
            if prev.stdenv.hostPlatform.isDarwin then
              prev.inetutils.overrideAttrs (oldAttrs: {
                hardeningDisable = (oldAttrs.hardeningDisable or [ ]) ++ [ "format" ];
              })
            else
              prev.inetutils;
        })
      ];

      overlays = compatOverlays ++ [
        inputs.nixgl.overlay

        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = final.stdenv.hostPlatform.system;
            config = config;
            overlays = compatOverlays;
          };

          nur = import inputs.nur {
            nurpkgs = final;
            pkgs = final;
          };

          local = import ./pkgs final prev inputs;
        })
      ];

      mkHomeManagerModule =
        { hostname }:
        [
          inputs.sops-nix.homeManagerModules.sops
          inputs.niri.homeModules.niri
          ./home-manager/modules
          ./home-manager/config/machines/${hostname}.nix
          (if builtins.pathExists ./local.nix then ./local.nix else { })
          inputs.nix-index-database.homeModules.nix-index
        ];

      mkHomeManagerConfig =
        {
          hostname,
          username,
          system,
          homeDirectory,
        }:
        home-manager.lib.homeManagerConfiguration {
          # home-manager will be responsible for evaluating the nixpkgs.overlays.
          # We're passing legacyPackages here to avoid nixpkgs from being
          # evaluated twice.
          #
          # Ref:
          # home-manager/modules/modules.nix (`pkgPath = ...;')
          # home-manager/modules/misc/nixpkgs.nix (`import pkgPath ...;')
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            {
              nixpkgs.overlays = overlays;
              nixpkgs.config = config;
              programs.home-manager.enable = true;
              home.username = username;
              home.homeDirectory = homeDirectory;
              home.stateVersion = "25.11";
              news.display = "silent";
            }
          ]
          ++ (mkHomeManagerModule { inherit hostname; });
        };

      mkNixOSConfig =
        {
          hostname,
          username ? "sirn",
          homeDirectory ? "/home/${username}",
        }:
        {
          home-manager.useGlobalPkgs = false;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";
          home-manager.users.${username} = {
            imports = [
              (mkHomeManagerConfig { inherit username hostname homeDirectory; })
            ];
          };
        };

      mkHomeManagerLinuxConfig =
        {
          hostname,
          username ? "sirn",
          system ? "x86_64-linux",
          homeDirectory ? "/home/${username}",
          ...
        }:
        mkHomeManagerConfig {
          inherit
            hostname
            username
            system
            homeDirectory
            ;
        };

      mkHomeManagerDarwinConfig =
        {
          hostname,
          username ? "sirn",
          system ? "aarch64-darwin",
          homeDirectory ? "/Users/${username}",
          ...
        }:
        mkHomeManagerConfig {
          inherit
            hostname
            username
            system
            homeDirectory
            ;
        };

      # Helper for eachSystem pattern
      eachSystem =
        f:
        nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (
          system: f nixpkgs.legacyPackages.${system}
        );

      # Eval the treefmt configuration
      treefmtEval = eachSystem (
        pkgs:
        inputs.treefmt-nix.lib.evalModule pkgs {
          # Used to find the project root
          projectRootFile = "flake.nix";

          # Enable formatters
          programs.nixfmt = {
            enable = true;
            package = pkgs.nixfmt-rfc-style;
            strict = true;
          };

          programs.prettier.enable = true;
          programs.shfmt.enable = true;

          # Global settings
          settings = {
            excludes = [
              "*.sops.*"
              "flake.lock"
              "secrets/**"
            ];
          };
        }
      );
    in
    {
      # Home Manager module to be included by a standalnoe Home Manager
      homeConfigurations = {
        phoebe = mkHomeManagerLinuxConfig { hostname = "phoebe"; };
        polaris = mkHomeManagerLinuxConfig { hostname = "polaris"; };
        system76 = mkHomeManagerLinuxConfig { hostname = "system76"; };
        terra = mkHomeManagerLinuxConfig { hostname = "terra"; };
        theia = mkHomeManagerDarwinConfig { hostname = "theia"; };
        ws = mkHomeManagerLinuxConfig { hostname = "ws"; };
      };

      # NixOS module to be included by NixOS configuration
      nixosModules = {
        phoebe = mkNixOSConfig { hostname = "phoebe"; };
        polaris = mkNixOSConfig { hostname = "polaris"; };
        system76 = mkNixOSConfig { hostname = "system76"; };
        terra = mkNixOSConfig { hostname = "terra"; };
        ws = mkNixOSConfig { hostname = "ws"; };
      };

      # Apps output for nix run path:.#treefmt
      apps = eachSystem (pkgs: {
        treefmt = {
          type = "app";
          program = "${treefmtEval.${pkgs.system}.config.build.wrapper}/bin/treefmt";
        };
      });
    };
}
