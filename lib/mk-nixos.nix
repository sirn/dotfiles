{
  inputs,
  nixpkgs,
  overlays,
  nixpkgsConfig,
  stateVersion,
  localCfg,
  mkMicroVM,
  mkHomeManagerModules,
  mkHomeManagerBaseModule,
}:
let
  optionalPath = p: if builtins.pathExists p then p else { };
in
{
  hostname,
  username ? "sirn",
  system ? "x86_64-linux",
  homeDirectory ? "/home/${username}",
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

    # NixOS Generate Config (per-machine, gitignored)
    (optionalPath ../configuration.nix)
    (optionalPath ../hardware-configuration.nix)

    # Configurations
    ../nixos/modules
    localCfg.nixos
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
        ++ (mkHomeManagerModules hostname);
      }
    )
  ];
}
