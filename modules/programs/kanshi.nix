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
        apple_pro_display_xdr = {
          criteria = "Apple Computer Inc ProDisplayXDR 0x00001F07";
          mode = "6016x3384";
          position = "0,0";
          scale = 2.0;
        };
        asus_pa148 = {
          criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
          mode = "1920x1080";
          position = "0,0";
          scale = 1.0;
        };
        fallback = {
          criteria = "*";
          mode = "3840x2160";
          position = "0,0";
          scale = 2.0;
        };
      in
      {
        "main_dual" = {
          outputs = [
            (apple_pro_display_xdr // { position = "1920,0"; })
            fallback
          ];
        };
        "main_xdr" = {
          outputs = [
            apple_pro_display_xdr
          ];
        };
        "main_pa148" = {
          outputs = [
            asus_pa148
          ];
        };
        "main_fallback" = {
          outputs = [
            fallback
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
  wayexec.services =
    mkIf (!config.services.kanshi.enable) {
      kanshi = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          ${pkgs.kanshi}/bin/kanshi
        '';
      };
    };
}
