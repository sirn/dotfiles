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

      clipboard = {
        copy = {
          command = mkOption {
            type = types.str;
            default =
              if pkgs.stdenv.isDarwin
              then "pbcopy"
              else "${pkgs.xclip}/bin/xclip -selection clipboard";
            description = ''
              Command to copy to clipboard
            '';
          };
        };

        paste = {
          command = mkOption {
            type = types.str;
            default =
              if pkgs.stdenv.isDarwin
              then "pbpaste"
              else "${pkgs.xclip}/bin/xclip -selection clipboard -o";
            description = ''
              Command to paste from clipboard
            '';
          };
        };
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
