{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      overlays = [
        (final: prev: {
          local = import ./nixpkgs/pkgs { pkgs = prev; };
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
            home.stateVersion = "22.05";
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
            ./nixpkgs/lib/darwin.nix
            ./nixpkgs/lib/machine.nix
            ./nixpkgs/lib/runit.nix
            ./nixpkgs/modules/machines/${hostname}.nix
          ];
        };
    in
    {
      homeConfigurations = {
        apyx = mkConfig {
          hostname = "apyx";
        };
        gemini = mkConfig {
          hostname = "gemini";
        };
        pyxis = mkConfig {
          hostname = "pyxis";
          system = "aarch64-darwin";
          homeDirectory = "/Users/sirn";
        };
        theia = mkConfig {
          hostname = "theia";
          system = "aarch64-darwin";
          homeDirectory = "/Users/sirn";
        };
        ws = mkConfig {
          hostname = "ws";
        };
      };
    };
}
