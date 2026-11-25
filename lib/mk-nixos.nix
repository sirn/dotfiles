{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  mkMicroVM,
  homeManagerModules,
  mkHomeManagerBaseModule,
  modules,
}:
{
  username ? "sirn",
  system ? "x86_64-linux",
  homeDirectory ? "/home/${username}",
  extraModules ? [ ],
  extraHomeModules ? [ ],
}:
let
  nixosConfig = modules.nixos.config;
  hmConfig = modules.homeManager.config;
in
nixpkgs.lib.nixosSystem {
  inherit system;

  specialArgs = {
    inherit (inputs) nixos-hardware microvm nixvirt;
    inherit mkMicroVM nixosConfig;
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
        home-manager.extraSpecialArgs = { inherit hmConfig; };
        home-manager.users.${username}.imports = [
          (mkHomeManagerBaseModule { inherit username homeDirectory; })
        ]
        ++ homeManagerModules
        ++ extraHomeModules;
      }
    )
  ]
  ++ extraModules;
}
