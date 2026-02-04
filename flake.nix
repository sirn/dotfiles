{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    sops-nix.url = "github:Mic92/sops-nix";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    nixgl.url = "github:nix-community/nixGL";

    niri.url = "github:sodiboo/niri-flake";

    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      config = {
        allowUnfree = true;
      };

      overlays = [
        inputs.nixgl.overlay
        inputs.emacs-overlay.overlay
        (final: prev: {
          # darwin is known to be crashy when doing fork+exec
          # something changed in nix that cause these two tests to fail
          # https://github.com/dvarrazzo/py-setproctitle/issues/113
          # TODO: revisit after https://github.com/NixOS/nixpkgs/issues/479313
          pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
            (py-final: py-prev: {
              setproctitle = py-prev.setproctitle.overrideAttrs (oldAttrs: {
                disabledTests = oldAttrs.disabledTests ++
                  (py-prev.lib.optionals py-prev.stdenv.hostPlatform.isDarwin [
                    "test_fork_segfault"
                    "test_thread_fork_segfault"
                  ]);
              });
            })
          ];

          unstable = import inputs.nixpkgs-unstable {
            system = final.stdenv.hostPlatform.system;
            config = config;
          };

          nur = import inputs.nur {
            nurpkgs = final;
            pkgs = final;
          };

          local = import ./pkgs final prev inputs;
        })
      ];

      mkConfig =
        { hostname, username, system, homeDirectory, ... }:
        let
          defaultConfig = { pkgs, ... }: {
            nixpkgs.overlays = overlays;
            nixpkgs.config = config;
            programs.home-manager.enable = true;
            home.username = username;
            home.homeDirectory = homeDirectory;
            home.stateVersion = "25.11";
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
            inputs.sops-nix.homeManagerModules.sops
            inputs.niri.homeModules.niri
            ./lib/flatpak.nix
            ./lib/machine.nix
            ./modules/machines/${hostname}.nix
            (if builtins.pathExists ./local.nix then ./local.nix else { })
            inputs.nix-index-database.homeModules.nix-index
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
        phoebe = mkLinuxConfig { hostname = "phoebe"; };
        polaris = mkLinuxConfig { hostname = "polaris"; };
        terra = mkLinuxConfig { hostname = "terra"; };
        theia = mkDarwinConfig { hostname = "theia"; };
        ws = mkLinuxConfig { hostname = "ws"; };
      };
    };
}
