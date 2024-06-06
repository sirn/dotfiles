{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      overlays = [
        (final: prev: {
          local = import ./pkgs {
            pkgs = final;
            lib = prev.lib;
          };

          unstable = import nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };
        })
      ];

      mkConfig =
        { hostname, username, system, homeDirectory, ... }:
        let
          defaultConfig = { pkgs, ... }: {
            nixpkgs.overlays = overlays;
            nixpkgs.config.allowUnfree = true;
            programs.home-manager.enable = true;
            targets.genericLinux.enable = pkgs.stdenv.isLinux;
            home.username = username;
            home.homeDirectory = homeDirectory;
            home.stateVersion = "23.11";
            news.display = "silent";
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
            ./lib/runit/runit.nix
            ./lib/runit/wayexec.nix
            ./modules/machines/${hostname}.nix
          ];
        };

      mkLinuxConfig =
        { hostname
        , username ? "sirn"
        , system ? "x86_64-linux"
        , homeDirectory ? "/home/${username}"
        , ...
        }:
        mkConfig {
          inherit hostname username system homeDirectory;
        };

      mkLinuxArmConfig =
        { hostname
        , username ? "sirn"
        , system ? "aarch64-linux"
        , homeDirectory ? "/home/${username}"
        , ...
        }:
        mkConfig {
          inherit hostname username system homeDirectory;
        };

      mkDarwinConfig =
        { hostname
        , username ? "sirn"
        , system ? "aarch64-darwin"
        , homeDirectory ? "/Users/${username}"
        , ...
        }:
        mkConfig {
          inherit hostname username system homeDirectory;
        };
    in
    {
      homeConfigurations = {
        helios = mkLinuxConfig { hostname = "helios"; };
        phoebe = mkLinuxConfig { hostname = "phoebe"; };
        pyxis = mkDarwinConfig { hostname = "pyxis"; };
        terra = mkLinuxConfig { hostname = "terra"; };
        theia = mkDarwinConfig { hostname = "theia"; };
        vega = mkLinuxConfig { hostname = "vega"; };
        ws = mkLinuxArmConfig { hostname = "ws"; };
      };
    };
}
