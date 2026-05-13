{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.11";
    };

    nixpkgs-unstable = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    ## Home Manager
    ##

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # TODO: remove after Home Manager > 25.11
    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
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
    };

    # Definition of Nix systems
    systems = {
      url = "github:nix-systems/default-linux";
    };

    # Definition of Linux Nix systems
    systems-linux = {
      url = "github:nix-systems/default-linux";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
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
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      nixpkgsConfig = {
        allowUnfree = true;
      };

      stateVersion = "25.11";

      optionalPath = p: if builtins.pathExists p then p else { };

      localCfg =
        if builtins.pathExists ./local/default.nix then
          import ./local/default.nix
        else
          {
            nixos = { };
            home = { };
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

      compatOverlays = [ ];

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

          local = import ./pkgs final prev inputs;
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

          # TODO: remove after Home Manager > 25.11
          { disabledModules = [ "services/swww.nix" ]; }
          "${inputs.home-manager-unstable}/modules/services/awww.nix"

          # Configurations
          ./home-manager/modules
          localCfg.home
          profile.home
        ];

      # Returns the base Home Manager configuration module
      mkHomeManagerBaseModule =
        { username, homeDirectory }:
        {
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

            # TODO: remove after NixOS > 25.11
            { disabledModules = [ "services/hardware/tlp.nix" ]; }
            "${inputs.nixpkgs-unstable}/nixos/modules/services/hardware/tlp.nix"

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
      # Home Manager module to be included by a standalone Home Manager
      homeConfigurations = {
        phoebe = mkHomeManagerLinux { hostname = "phoebe"; };
        polaris = mkHomeManagerLinux { hostname = "polaris"; };
        system76 = mkHomeManagerLinux { hostname = "system76"; };
        terra = mkHomeManagerLinux { hostname = "terra"; };
        theia = mkHomeManagerDarwin { hostname = "theia"; };
        ws = mkHomeManagerLinux { hostname = "ws"; };
      };

      nixConfig = {
        extra-substituters = [
          "https://noctalia.cachix.org"
          "https://microvm.cachix.org"
        ];
        extra-trusted-public-keys = [
          "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
          "microvm.cachix.org-1:oXnBc6hRE3eX5rSYdRyMYXnfzcCxC7yKPTbZXALsqys="
        ];
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
      apps = eachSystem (pkgs: {
        treefmt = {
          type = "app";
          program = "${treefmtEval.${pkgs.system}.config.build.wrapper}/bin/treefmt";
        };
      });
    };
}
