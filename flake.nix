{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      overlays = [
        (final: prev: {
          local = import ./pkgs { pkgs = prev; };
          unstable = import nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };
        })
      ];

      mkConfig =
        { hostname
        , username ? "sirn"
        , system ? "x86_64-linux"
        , homeDirectory ? "/home/${username}"
        , ...
        }:
        let
          defaultConfig = { pkgs, ... }: {
            nixpkgs.overlays = overlays;
            nixpkgs.config.allowUnfree = true;
            programs.home-manager.enable = true;
            targets.genericLinux.enable = pkgs.stdenv.isLinux;
            home.username = username;
            home.homeDirectory = homeDirectory;
            home.stateVersion = "23.05";
          };
        in
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
            defaultConfig
            ./lib/flatpak.nix
            ./lib/machine.nix
            ./lib/runit.nix
            ./modules/machines/${hostname}.nix
          ];
        };
    in
    {
      homeConfigurations = {
        helios = mkConfig {
          hostname = "helios";
        };
        phoebe = mkConfig {
          hostname = "phoebe";
        };
        pyxis = mkConfig {
          hostname = "pyxis";
          system = "aarch64-darwin";
          homeDirectory = "/Users/sirn";
        };
        terra = mkConfig {
          hostname = "terra";
        };
        theia = mkConfig {
          hostname = "theia";
          system = "aarch64-darwin";
          homeDirectory = "/Users/sirn";
        };
        vega = mkConfig {
          hostname = "vega";
        };
        ws = mkConfig {
          hostname = "ws";
        };
      };
    };
}
