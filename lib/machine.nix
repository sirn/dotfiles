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

      isLaptop = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Indicates whether the machine is a laptop.
        '';
      };

      desktop = {
        defaultFonts = {
          sansSerif = mkOption {
            type = types.str;
            default = "Noto Sans";
            description = ''
              Default Sans Serif font.
            '';
          };

          serif = mkOption {
            type = types.str;
            default = "Noto Serif";
            description = ''
              Default Serif font.
            '';
          };

          monospace = mkOption {
            type = types.str;
            default = "Hack";
            description = ''
              Default Monospace font.
            '';
          };
        };
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
