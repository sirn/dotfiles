{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    machine = {
      isNixOS = mkOption {
        type = types.bool;
        default = isLinux;
        description = ''
          Indicates whether the machine is a NixOS system.
        '';
      };
    };
  };

  config = {
    systemd = mkIf config.systemd.user.enable {
      user = {
        startServices = true;
      };
    };
  };
}
