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

      xdgRuntimePrefix = mkOption {
        type = types.lines;
        default = "/run/user";
        description = ''
          Prefix for XDG_RUNTIME_DIR without the user ID.
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
