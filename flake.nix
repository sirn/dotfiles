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

    # 1Password secrets for direnv.
    direnv-op = {
      url = "github:direnv-contrib/direnv-op";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
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
      inputs.systems.follows = "systems-default";
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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
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

      overlays = import ./pkgs/overlays.nix { inherit inputs nixpkgsConfig; };

      builders = import ./lib {
        inherit
          inputs
          nixpkgs
          overlays
          nixpkgsConfig
          stateVersion
          ;
      };

      # Helper for eachSystem pattern
      eachSystem = f: nixpkgs.lib.genAttrs (import inputs.systems-default) (system: f system);
    in
    {
      lib = {
        inherit (builders)
          modules
          mkNixOS
          mkHomeManagerLinux
          mkHomeManagerDarwin
          mkMicroVM
          ;
      };

      # Generic stubs for local evaluation. Machine profiles live in the
      # private repository, which passes them as extra modules.
      homeConfigurations = {
        stub = builders.mkHomeManagerLinux { extraModules = [ ./home-manager/config/common.nix ]; };
        stub-darwin = builders.mkHomeManagerDarwin {
          extraModules = [
            ./home-manager/config/common.nix
            ./home-manager/config/common-darwin.nix
          ];
        };
      };

      nixosConfigurations = {
        stub = builders.mkNixOS {
          extraModules = [
            ./nixos/config/common.nix
            {
              fileSystems."/" = {
                device = "/dev/disk/by-label/nixos";
                fsType = "ext4";
              };
              boot.loader.grub.enable = false;
            }
          ];
          extraHomeModules = [ ./home-manager/config/common.nix ];
        };
      };

      # Apps output for nix run .#treefmt
      apps = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        in
        {
          treefmt = {
            type = "app";
            program = "${treefmtEval.config.build.wrapper}/bin/treefmt";
          };
        }
      );

      # Expose the overlay packages as derivations so their passthru attrs
      # (npmDeps, cargoDeps, goModules) are reachable as .#<pkg>.<attr> by the
      # pkgs/by-name/*/update.sh scripts. The private configuration also seeds
      # this output into its binary cache.
      packages = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = nixpkgsConfig;
            inherit overlays;
          };
        in
        nixpkgs.lib.filterAttrs (_: v: nixpkgs.lib.isDerivation v) (pkgs.local // pkgs.llm-agents)
      );
    };
}
