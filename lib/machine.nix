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
        startServices = true;
      };
    };
  };
}
