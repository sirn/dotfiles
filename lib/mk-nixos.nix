{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  mkMicroVM,
  mkHomeManagerModules,
  mkHomeManagerBaseModule,
}:
{
  hostname,
  username ? "sirn",
  system ? "x86_64-linux",
  homeDirectory ? "/home/${username}",
  extraModules ? [ ],
  extraHomeModules ? [ ],
}:
let
  profile = import ../profiles/${hostname}.nix;
in
nixpkgs.lib.nixosSystem {
  inherit system;

  specialArgs = {
    inherit (inputs) nixos-hardware microvm nixvirt;
    inherit mkMicroVM;
  };

  modules = [
    {
      nixpkgs.overlays = overlays;
      nixpkgs.config = nixpkgsConfig;
      system.stateVersion = stateVersion;
    }

    # NixOS modules
    inputs.microvm.nixosModules.host
    inputs.nixvirt.nixosModules.default
    inputs.sops-nix.nixosModules.sops

    # Configurations
    ../nixos/modules
    profile.nixos

    # Home Manager
    inputs.home-manager.nixosModules.home-manager
    (
      { lib, pkgs, ... }:
      let
        hm-backup = pkgs.writeScriptBin "hm-backup" ''
          #!${pkgs.runtimeShell}
          mv "$1" "$1.backup.$(date +%s)"
        '';
      in
      {
        home-manager.useGlobalPkgs = false;
        home-manager.useUserPackages = true;
        home-manager.backupCommand = lib.getExe hm-backup;
        home-manager.users.${username}.imports = [
          (mkHomeManagerBaseModule { inherit username homeDirectory; })
        ]
        ++ (mkHomeManagerModules hostname)
        ++ extraHomeModules;
      }
    )
  ]
  ++ extraModules;
}
