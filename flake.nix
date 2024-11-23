{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    jujutsu.url = "github:martinvonz/jj";
    wezterm.url = "github:wez/wezterm?dir=nix";
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      config = {
        allowUnfree = true;
        permittedInsecurePackages = [
          "openssl-1.1.1w" # sublime4
        ];
      };

      overlays = [
        (final: prev: {
          local = import ./pkgs {
            pkgs = final;
            lib = prev.lib;
          };
        })
        (final: prev: {
          unstable = import nixpkgs-unstable {
            system = final.system;
            config = config;
          };
        })
        (final: prev: {
          nightlies = {
            jujutsu = inputs.jujutsu.packages.${final.system}.jujutsu;
            wezterm = inputs.wezterm.packages.${final.system}.default;
          };
        })
      ];

      mkConfig =
        { hostname, username, system, homeDirectory, ... }:
        let
          defaultConfig = { pkgs, ... }: {
            nixpkgs.overlays = overlays;
            nixpkgs.config = config;
            programs.home-manager.enable = true;
            targets.genericLinux.enable = pkgs.stdenv.isLinux;
            home.username = username;
            home.homeDirectory = homeDirectory;
            home.stateVersion = "24.11";
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
        polaris = mkLinuxConfig { hostname = "polaris"; };
        pyxis = mkDarwinConfig { hostname = "pyxis"; };
        terra = mkLinuxConfig { hostname = "terra"; };
        theia = mkDarwinConfig { hostname = "theia"; };
        vega = mkLinuxConfig { hostname = "vega"; };
        ws = mkLinuxConfig { hostname = "ws"; };
      };
    };
}
