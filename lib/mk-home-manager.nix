{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  localCfg,
}:
let
  mkHomeManagerModules =
    hostname:
    let
      profile = import ../profiles/${hostname}.nix;
    in
    [
      # Home Manager modules
      inputs.niri.homeModules.niri
      inputs.nix-index-database.homeModules.nix-index
      inputs.noctalia.homeModules.default
      inputs.sops-nix.homeManagerModules.sops

      # Configurations
      ../home-manager/modules
      localCfg.home
      profile.home
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
      hostname,
      username,
      system,
      homeDirectory,
    }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      modules = [
        (mkHomeManagerBaseModule { inherit username homeDirectory; })
      ]
      ++ (mkHomeManagerModules hostname);
    };
in
{
  inherit mkHomeManagerModules mkHomeManagerBaseModule;

  mkHomeManagerLinux =
    {
      hostname,
      username ? "sirn",
      system ? "x86_64-linux",
      homeDirectory ? "/home/${username}",
      ...
    }:
    mkHomeManager {
      inherit
        hostname
        username
        system
        homeDirectory
        ;
    };

  mkHomeManagerDarwin =
    {
      hostname,
      username ? "sirn",
      system ? "aarch64-darwin",
      homeDirectory ? "/Users/${username}",
      ...
    }:
    mkHomeManager {
      inherit
        hostname
        username
        system
        homeDirectory
        ;
    };
}
