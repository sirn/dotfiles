{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
}:
let
  modules = import ./modules.nix { inherit (nixpkgs) lib; };

  mkMicroVM = import ./mk-microvm.nix {
    inherit (inputs)
      microvm
      home-manager
      direnv-op
      sops-nix
      niri
      nix-index-database
      noctalia
      ;
    inherit overlays stateVersion modules;
  };

  hmBuilders = import ./mk-home-manager.nix {
    inherit
      inputs
      nixpkgs
      overlays
      nixpkgsConfig
      stateVersion
      modules
      ;
  };

  mkNixOS = import ./mk-nixos.nix {
    inherit
      inputs
      nixpkgs
      overlays
      nixpkgsConfig
      stateVersion
      mkMicroVM
      modules
      ;
    inherit (hmBuilders) homeManagerModules mkHomeManagerBaseModule;
  };
in
{
  inherit mkMicroVM mkNixOS;
  inherit (hmBuilders) mkHomeManagerLinux mkHomeManagerDarwin;

  inherit modules;
}
