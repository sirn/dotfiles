{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  modules,
}:
let
  hmConfig = modules.homeManager.config;

  homeManagerModules = [
    # Home Manager modules
    inputs.direnv-op.homeModules.default
    inputs.niri.homeModules.niri
    inputs.nix-index-database.homeModules.nix-index
    inputs.noctalia.homeModules.default
    inputs.sops-nix.homeManagerModules.sops

    # Configurations
    ../home-manager/modules
  ];

  mkHomeManagerBaseModule = { username, homeDirectory }: {
    nixpkgs.overlays = overlays;
    nixpkgs.config = nixpkgsConfig;
    programs.home-manager.enable = true;
    home.username = username;
    home.homeDirectory = homeDirectory;
    home.stateVersion = stateVersion;
    news.display = "silent";
  };

  mkHomeManager =
    {
      username,
      system,
      homeDirectory,
      extraModules ? [ ],
    }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = { inherit hmConfig; };
      modules = [
        (mkHomeManagerBaseModule { inherit username homeDirectory; })
      ]
      ++ homeManagerModules
      ++ extraModules;
    };
in
{
  inherit homeManagerModules mkHomeManagerBaseModule;

  mkHomeManagerLinux =
    {
      username ? "sirn",
      system ? "x86_64-linux",
      homeDirectory ? "/home/${username}",
      extraModules ? [ ],
    }:
    mkHomeManager {
      inherit
        username
        system
        homeDirectory
        extraModules
        ;
    };

  mkHomeManagerDarwin =
    {
      username ? "sirn",
      system ? "aarch64-darwin",
      homeDirectory ? "/Users/${username}",
      extraModules ? [ ],
    }:
    mkHomeManager {
      inherit
        username
        system
        homeDirectory
        extraModules
        ;
    };
}
