{ config, lib, pkgs, ... }:

let
  inherit (lib) concatStringsSep mapAttrsToList mkIf optionalString;

  swaymsgBin =
    if config.wayland.windowManager.sway.package != null
    then "${config.wayland.windowManager.sway.package}/bin/swaymsg"
    else "swaymsg";

  # Copied from home-manager/modules/services/kanshi.nix
  outputStr =
    { criteria, status, mode, position, scale, transform, ... }:
    ''output "${criteria}"'' + optionalString (status != null) " ${status}"
    + optionalString (mode != null) " mode ${mode}"
    + optionalString (position != null) " position ${position}"
    + optionalString (scale != null) " scale ${toString scale}"
    + optionalString (transform != null) " transform ${transform}";

  profileStr = name:
    { outputs, exec, ... }: ''
      profile ${name} {
        ${
          concatStringsSep "\n  "
          (map outputStr outputs ++ map (cmd: "exec ${cmd}") exec)
        }
      }
    '';
in
mkIf config.desktop.enable {
  services.kanshi = {
    enable = config.machine.isNixOS;

    profiles =
      let
        apple_pro_xdr_display = "Apple Computer Inc ProDisplayXDR 0x00001F07";
        lg_27uk650_w = "LG Electronics LG HDR 4K 0x00006393";
        asus_pa148 = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
      in
      {
        "main_dual" = {
          outputs = [
            {
              criteria = "${apple_pro_xdr_display}";
              mode = "6016x3384";
              position = "2560,0";
              scale = 2.0;
            }
            {
              criteria = "${lg_27uk650_w}";
              mode = "3840x2160";
              position = "0,0";
              scale = 1.5;
            }
          ];
          exec = [
            "${swaymsgBin} workspace 1, move workspace to output '\"${apple_pro_xdr_display}\"'"
            "${swaymsgBin} workspace 10, move workspace to output '\"${lg_27uk650_w}\"'"
          ];
        };
        "main_single_lg" = {
          outputs = [
            {
              criteria = "${lg_27uk650_w}";
              mode = "3840x2160";
              position = "0,0";
              scale = 1.5;
            }
          ];
        };
        "main_single_xdr" = {
          outputs = [
            {
              criteria = "${apple_pro_xdr_display}";
              mode = "6016x3384";
              position = "0,0";
              scale = 2.0;
            }
          ];
        };
        "main_single_pa148" = {
          outputs = [
            {
              criteria = "${asus_pa148}";
              mode = "1920x1080";
              position = "0,0";
              scale = 1.5;
            }
          ];
        };
      };
  };

  # non-NixOS
  xdg.configFile = mkIf (!config.services.kanshi.enable) {
    "kanshi/config" = {
      text = ''
        ${concatStringsSep "\n" (mapAttrsToList profileStr config.services.kanshi.profiles)}
        ${config.services.kanshi.extraConfig}
      '';
    };
  };

  # non-NixOS; assume no systemd
  wayland.windowManager.sway =
    mkIf (!config.services.kanshi.enable) {
      config = {
        startup = [
          {
            always = true;
            command = ''
              ${pkgs.writeScriptBin "start-kanshi" ''
                #!${pkgs.bash}/bin/bash
                pkill -Af kanshi

                run_and_disown() {
                  "$@" &
                  sleep 0.5
                  disown
                }

                run_and_disown ${pkgs.kanshi}/bin/kanshi
              ''}/bin/start-kanshi
            '';
          }
        ];
      };
    };
}
