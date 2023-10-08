{ config, lib, pkgs, ... }:

with lib; {
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
      };
    };
  };
}
