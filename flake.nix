{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.11";
    };

    nixpkgs-unstable = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      config = {
        allowUnfree = true;
      };

      compatOverlays = [
        (final: prev: {
          # yt-dlp includes secretstorage (for Gnome keyring) which depends on jeepney.
          # jeepney tests fail on Darwin due to missing D-Bus session bus.
          # secretstorage is only needed for --cookies-from-browser on Linux.
          # Remove once https://github.com/NixOS/nixpkgs/issues/493775 is in unstable.
          yt-dlp =
            if prev.stdenv.hostPlatform.isDarwin then
              prev.yt-dlp.overridePythonAttrs
                (oldAttrs: {
                  dependencies = prev.lib.filter
                    (p: !(prev.lib.elem (p.pname or "") [ "cffi" "secretstorage" ]))
                    oldAttrs.dependencies;
                })
            else
              prev.yt-dlp;
        })

        (final: prev: {
          # inetutils 2.7 has a format string bug that fails with strict compiler flags
          # See: https://github.com/NixOS/nixpkgs/issues/488689
          inetutils =
            if prev.stdenv.hostPlatform.isDarwin then
              prev.inetutils.overrideAttrs
                (oldAttrs: {
                  hardeningDisable = (oldAttrs.hardeningDisable or [ ]) ++ [ "format" ];
                })
            else
              prev.inetutils;
        })
      ];

      overlays = compatOverlays ++ [
        inputs.nixgl.overlay
        inputs.emacs-overlay.overlay

        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = final.stdenv.hostPlatform.system;
            config = config;
            overlays = compatOverlays;
          };

          nur = import inputs.nur {
            nurpkgs = final;
            pkgs = final;
          };

          local = import ./pkgs final prev inputs;
        })
      ];

      mkModules =
        { hostname }:
        [
          inputs.sops-nix.homeManagerModules.sops
          inputs.niri.homeModules.niri
          ./modules
          ./config/machines/${hostname}.nix
          (if builtins.pathExists ./local.nix then ./local.nix else { })
          inputs.nix-index-database.homeModules.nix-index
        ];

      mkDefaultConfig =
        { username
        , homeDirectory
        }:
        { pkgs, ... }: {
          nixpkgs.overlays = overlays;
          nixpkgs.config = config;
          programs.home-manager.enable = true;
          home.username = username;
          home.homeDirectory = homeDirectory;
          home.stateVersion = "25.11";
          news.display = "silent";
        };

      mkHomeConfig =
        { hostname
        , username
        , system
        , homeDirectory
        }:
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
            (mkDefaultConfig { inherit username homeDirectory; })
          ] ++ (mkModules { inherit hostname; });
        };

      mkNixOSConfig =
        { hostname
        , username ? "sirn"
        , homeDirectory ? "/home/${username}"
        }:
        {
          home-manager.useGlobalPkgs = false;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";
          home-manager.users.${username} = {
            imports = [
              (mkDefaultConfig { inherit username homeDirectory; })
            ] ++ (mkModules { inherit hostname; });
          };
        };

      mkLinuxConfig =
        { hostname
        , username ? "sirn"
        , system ? "x86_64-linux"
        , homeDirectory ? "/home/${username}"
        , ...
        }:
        mkHomeConfig {
          inherit hostname username system homeDirectory;
        };

      mkDarwinConfig =
        { hostname
        , username ? "sirn"
        , system ? "aarch64-darwin"
        , homeDirectory ? "/Users/${username}"
        , ...
        }:
        mkHomeConfig {
          inherit hostname username system homeDirectory;
        };
    in
    {
      homeConfigurations = {
        phoebe = mkLinuxConfig { hostname = "phoebe"; };
        polaris = mkLinuxConfig { hostname = "polaris"; };
        system76 = mkLinuxConfig { hostname = "system76"; };
        terra = mkLinuxConfig { hostname = "terra"; };
        theia = mkDarwinConfig { hostname = "theia"; };
        ws = mkLinuxConfig { hostname = "ws"; };
      };

      nixosModules = {
        phoebe = mkNixOSConfig { hostname = "phoebe"; };
        polaris = mkNixOSConfig { hostname = "polaris"; };
        system76 = mkNixOSConfig { hostname = "system76"; };
        terra = mkNixOSConfig { hostname = "terra"; };
        ws = mkNixOSConfig { hostname = "ws"; };
      };
    };
}
