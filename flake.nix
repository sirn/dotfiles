{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-26.05";
    };

    nixpkgs-unstable = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    ## Home Manager
    ##

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
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
      inputs.flake-utils.follows = "flake-utils";
    };

    # Multi-file formatting with Nix
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Hardware presets for NixOS
    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Definition of all Nix systems
    systems-default = {
      url = "github:nix-systems/default";
    };

    # Definition of Linux Nix systems
    systems-linux = {
      url = "github:nix-systems/default-linux";
    };

    # Flake parts library (shared to avoid duplicates)
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems-linux";
    };

    ## MicroVM
    ##

    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## LibVirt
    ##

    nixvirt = {
      url = "github:AshleyYakeley/NixVirt";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Package overlays
    ##

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.noctalia-qs = {
        inputs.treefmt-nix.follows = "treefmt-nix";
        inputs.systems.follows = "systems-linux";
      };
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.systems.follows = "systems-default";
      inputs.flake-parts.follows = "flake-parts";
    };
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      nixpkgsConfig = {
        allowUnfree = true;
      };

      stateVersion = "26.05";

      optionalPath = p: if builtins.pathExists p then p else { };

      localCfg =
        let
          cfg = if builtins.pathExists ./local/default.nix then import ./local/default.nix else { };
        in
        {
          nixos = cfg.nixos or { };
          home = cfg.home or { };
        };

      mkMicroVM = import ./nixos/lib/mk-microvm.nix {
        inherit (inputs)
          microvm
          home-manager
          sops-nix
          niri
          nix-index-database
          noctalia
          ;
        inherit overlays stateVersion;
      };

      compatOverlays = [
        # Bun-compiled tailwindcss standalone is only linker-signed, which
        # amfid kills on recent macOS; trunk's tailwind hook then can't read
        # its version offline.
        #
        # TODO: revisit after macOS 27 is released.
        (
          final: prev:
          prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
            tailwindcss_4 = prev.tailwindcss_4.overrideAttrs (
              final': prev': {
                postFixup = (prev'.postFixup or "") + ''
                  /usr/bin/codesign -f -s - $out/bin/.tailwindcss-wrapped
                '';
              }
            );
          }
        )
      ];

      overlays = compatOverlays ++ [
        inputs.nixgl.overlay

        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = final.stdenv.hostPlatform.system;
            config = nixpkgsConfig;
            overlays = compatOverlays;
          };

          nur = import inputs.nur {
            nurpkgs = final;
            pkgs = final;
          };

          llm-agents = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system};

          local = (import ./pkgs final prev inputs).${final.stdenv.hostPlatform.system};
        })
      ];

      # Returns a list of Home Manager modules
      mkHomeManagerModules =
        hostname:
        let
          profile = import ./profiles/${hostname}.nix;
        in
        [
          # Home Manager modules
          inputs.niri.homeModules.niri
          inputs.nix-index-database.homeModules.nix-index
          inputs.noctalia.homeModules.default
          inputs.sops-nix.homeManagerModules.sops

          # Configurations
          ./home-manager/modules
          localCfg.home
          profile.home
        ];

      # Returns the base Home Manager configuration module
      mkHomeManagerBaseModule = { username, homeDirectory }: {
        nixpkgs.overlays = overlays;
        nixpkgs.config = nixpkgsConfig;
        programs.home-manager.enable = true;
        home.username = username;
        home.homeDirectory = homeDirectory;
        home.stateVersion = stateVersion;
        news.display = "silent";
      };

      # Builds a standalone Home Manager configuration
      mkHomeManager =
        {
          hostname,
          username,
          system,
          homeDirectory,
        }:
        inputs.home-manager.lib.homeManagerConfiguration {
          # home-manager will be responsible for evaluating the nixpkgs.overlays.
          # We're passing legacyPackages here to avoid nixpkgs from being
          # evaluated twice.
          #
          # Ref:
          # home-manager/modules/modules.nix (`pkgPath = ...;')
          # home-manager/modules/misc/nixpkgs.nix (`import pkgPath ...;')
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            (mkHomeManagerBaseModule { inherit username homeDirectory; })
          ]
          ++ (mkHomeManagerModules hostname);
        };

      mkNixOS =
        {
          hostname,
          username ? "sirn",
          system ? "x86_64-linux",
          homeDirectory ? "/home/${username}",
        }:
        let
          profile = import ./profiles/${hostname}.nix;
        in
        nixpkgs.lib.nixosSystem {
          inherit system;

          specialArgs = {
            inherit (inputs) nixos-hardware microvm nixvirt;
            inherit mkMicroVM;
          };

          modules = [
            {
              nixpkgs.overlays = overlays;
              nixpkgs.config = nixpkgsConfig;
              system.stateVersion = stateVersion;
            }

            # NixOS modules
            inputs.microvm.nixosModules.host
            inputs.nixvirt.nixosModules.default
            inputs.sops-nix.nixosModules.sops

            # NixOS Generate Config (per-machine, gitignored)
            (optionalPath ./configuration.nix)
            (optionalPath ./hardware-configuration.nix)

            # Configurations
            ./nixos/modules
            localCfg.nixos
            profile.nixos

            # Home Manager
            inputs.home-manager.nixosModules.home-manager
            (
              { lib, pkgs, ... }:
              let
                hm-backup = pkgs.writeScriptBin "hm-backup" ''
                  #!${pkgs.runtimeShell}
                  mv "$1" "$1.backup.$(date +%s)"
                '';
              in
              {
                home-manager.useGlobalPkgs = false;
                home-manager.useUserPackages = true;
                home-manager.backupCommand = lib.getExe hm-backup;
                home-manager.users.${username}.imports = [
                  (mkHomeManagerBaseModule { inherit username homeDirectory; })
                ]
                ++ (mkHomeManagerModules hostname);
              }
            )
          ];
        };

      mkHomeManagerLinux =
        {
          hostname,
          username ? "sirn",
          system ? "x86_64-linux",
          homeDirectory ? "/home/${username}",
          ...
        }:
        mkHomeManager {
          inherit
            hostname
            username
            system
            homeDirectory
            ;
        };

      mkHomeManagerDarwin =
        {
          hostname,
          username ? "sirn",
          system ? "aarch64-darwin",
          homeDirectory ? "/Users/${username}",
          ...
        }:
        mkHomeManager {
          inherit
            hostname
            username
            system
            homeDirectory
            ;
        };

      # Helper for eachSystem pattern
      eachSystem = f: nixpkgs.lib.genAttrs (import inputs.systems-default) (system: f system);
    in
    {
      # Home Manager module to be included by a standalone Home Manager
      homeConfigurations = {
        phoebe = mkHomeManagerLinux { hostname = "phoebe"; };
        polaris = mkHomeManagerLinux { hostname = "polaris"; };
        system76 = mkHomeManagerLinux { hostname = "system76"; };
        terra = mkHomeManagerLinux { hostname = "terra"; };
        theia = mkHomeManagerDarwin { hostname = "theia"; };
        ws = mkHomeManagerLinux { hostname = "ws"; };
      };

      # NixOS configuration for NixOS
      nixosConfigurations = {
        phoebe = mkNixOS { hostname = "phoebe"; };
        polaris = mkNixOS { hostname = "polaris"; };
        system76 = mkNixOS { hostname = "system76"; };
        terra = mkNixOS { hostname = "terra"; };
        ws = mkNixOS { hostname = "ws"; };
      };

      # Apps output for nix run path:.#treefmt
      apps = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
            projectRootFile = "flake.nix";

            # Enable formatters
            programs.nixfmt = {
              enable = true;
              package = pkgs.nixfmt;
              strict = true;
            };

            programs.prettier = {
              enable = true;
              settings.proseWrap = "never";
            };

            programs.shfmt.enable = true;

            # Global settings
            settings = {
              excludes = [
                "*.sops.*"
                "flake.lock"
                "secrets/**"
              ];
            };
          };
        in
        {
          treefmt = {
            type = "app";
            program = "${treefmtEval.config.build.wrapper}/bin/treefmt";
          };
        }
      );

      # Expose all local packages from the overlay as derivations so their
      # passthru attrs (npmDeps, cargoDeps, goModules) are reachable as
      # path:.#<pkg>.<attr> by the pkgs/by-name/*/update.sh scripts.
      packages = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = nixpkgsConfig;
            inherit overlays;
          };
        in
        nixpkgs.lib.filterAttrs (_: v: nixpkgs.lib.isDerivation v) pkgs.local
      );
    };
}
