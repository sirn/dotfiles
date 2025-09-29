{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    sops-nix.url = "github:Mic92/sops-nix";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    nixgl.url = "github:nix-community/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";

    niri.url = "github:sodiboo/niri-flake";
    niri.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, nixpkgs-unstable, sops-nix, home-manager, nix-index-database, nixgl, niri, ... }@inputs:
    let
      config = {
        allowUnfree = true;
        permittedInsecurePackages = [
          "openssl-1.1.1w" # sublime4
        ];
      };

      overlays = [
        nixgl.overlay
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
        # Workaround crashes in Chromium with fractional scaling
        # https://bugs.launchpad.net/ubuntu/+source/wlroots/+bug/2122790
        # https://gitlab.freedesktop.org/wlroots/wlroots/-/issues/4015
        # https://github.com/swaywm/sway/issues/8194
        #
        # TODO: remove >= 25.11 (or once NixOS sway switch to wlroots 0.19)
        (final: prev: {
          wlroots_0_18 = prev.wlroots_0_18.overrideAttrs (old: {
            patches = (old.patches or [ ]) ++ [
              (prev.fetchpatch {
                url = "https://bugs.launchpad.net/bugs/2122790/+attachment/5909064/+files/render-pass-ensure-the-precision-is-consistent.patch";
                sha256 = "sha256-9dXZ7F+iv5JeAZBUnUp3CZSZ2QWvs0wGYGmcqHTqfJc=";
              })
            ];
          });
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
            home.stateVersion = "25.05";
            news.display = "silent";
          };
        in
        home-manager.lib.homeManagerConfiguration rec {
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
            sops-nix.homeManagerModules.sops
            niri.homeModules.niri
            ./lib/flatpak.nix
            ./lib/machine.nix
            ./modules/machines/${hostname}.nix
            (if builtins.pathExists ./local.nix then ./local.nix else { })
            nix-index-database.homeModules.nix-index
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
        terra = mkLinuxConfig { hostname = "terra"; };
        theia = mkDarwinConfig { hostname = "theia"; };
        vega = mkLinuxConfig { hostname = "vega"; };
        ws = mkLinuxConfig { hostname = "ws"; };
      };
    };
}
