{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    machine = {
      nixos = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Indicates whether the machine is a NixOS system.
          '';
        };
      };

      runit = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Indiciates whether the machine is a Runit system.
          '';
        };
      };

      gui = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to enable GUI support. Unlike setting gtk.enable and/or qt.enable
            this option does not write any settings and intended solely to be consumed
            by other modules.
          '';
        };

        preferDark = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to prefers dark theme over the default theme.
          '';
        };
      };
    };
  };

  config = {
    systemd = mkIf config.machine.nixos.enable {
      user = {
        startServices = true;
      };
    };
  };
}
