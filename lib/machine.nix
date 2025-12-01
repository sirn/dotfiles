{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    machine = {
      isLaptop = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Indicates whether the machine is a laptop.
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
    systemd = mkIf config.systemd.user.enable {
      user = {
        startServices = "sd-switch";
      };
    };
  };
}
