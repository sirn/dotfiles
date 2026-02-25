{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    machine = {
      wrapLauncher = mkOption {
        type = types.functionTo types.path;
        default = x:
          if lib.isDerivation x
          then lib.getExe x
          else x;
        description = ''
          Function to wrap a launcher command. Accepts either a package
          (will use lib.getExe) or a path string.
        '';
      };

      interactiveShell = mkOption {
        type = types.str;
        default = lib.getExe pkgs.bash;
        description = ''
          Default interactive shell.
        '';
      };
    };
  };

  config = {
    lib.machine.wrapLauncher = config.machine.wrapLauncher;

    systemd = mkIf config.systemd.user.enable {
      user = {
        startServices = "sd-switch";
      };
    };
  };
}
