{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      overlays = [
        (final: prev: { local = import ./nixpkgs/pkgs { pkgs = prev; }; })
        (final: prev: { unstable = nixpkgs-unstable.legacyPackages.${prev.system}; })
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
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit username system homeDirectory;
          pkgs = nixpkgs.legacyPackages.${system};

          configuration = {
            imports = [
              defaultConfig
              ./nixpkgs/lib/darwin.nix
              ./nixpkgs/lib/machine.nix
              ./nixpkgs/lib/runit.nix
              ./nixpkgs/modules/machines/${hostname}.nix
            ];
          };
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
