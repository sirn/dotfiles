{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  localCfg,
}:
let
  mkMicroVM = import ./mk-microvm.nix {
    inherit (inputs)
      microvm
      home-manager
      sops-nix
      niri
      nix-index-database
      noctalia
      ;
    inherit overlays stateVersion;
  };

  hmBuilders = import ./mk-home-manager.nix {
    inherit
      inputs
      nixpkgs
      overlays
      nixpkgsConfig
      stateVersion
      localCfg
      ;
  };

  mkNixOS = import ./mk-nixos.nix {
    inherit
      inputs
      nixpkgs
      overlays
      nixpkgsConfig
      stateVersion
      localCfg
      mkMicroVM
      ;
    inherit (hmBuilders) mkHomeManagerModules mkHomeManagerBaseModule;
  };
in
{
  inherit mkMicroVM mkNixOS;
  inherit (hmBuilders) mkHomeManagerLinux mkHomeManagerDarwin;
}
