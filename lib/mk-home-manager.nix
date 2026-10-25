{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
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
      extraModules ? [ ],
    }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      modules = [
        (mkHomeManagerBaseModule { inherit username homeDirectory; })
      ]
      ++ (mkHomeManagerModules hostname)
      ++ extraModules;
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
      extraModules ? [ ],
      ...
    }:
    mkHomeManager {
      inherit
        hostname
        username
        system
        homeDirectory
        extraModules
        ;
    };

  mkHomeManagerDarwin =
    {
      hostname,
      username ? "sirn",
      system ? "aarch64-darwin",
      homeDirectory ? "/Users/${username}",
      extraModules ? [ ],
      ...
    }:
    mkHomeManager {
      inherit
        hostname
        username
        system
        homeDirectory
        extraModules
        ;
    };
}
