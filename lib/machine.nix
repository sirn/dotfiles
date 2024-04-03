{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    machine = {
      isNixOS = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Indicates whether the machine is a NixOS system.
        '';
      };
    };
  };

  config = {
    systemd = mkIf config.machine.isNixOS {
      user = {
        startServices = true;
      };
    };
  };
}
