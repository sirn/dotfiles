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
        default = {
          criteria = "*";
          mode = "3840x2160";
          position = "0,0";
          scale = 2.0;
        };

        apple_pro_display_xdr = default // {
          criteria = "Apple Computer Inc ProDisplayXDR 0x00001F07";
          mode = "6016x3384";
        };

        lg_27uk650_w = default // {
          criteria = "LG Electronics LG HDR 4K 0x00006393";
          scale = 1.75;
        };

        innocn_pu15_pre = default // {
          criteria = "Beihai Century Joint Innovation Technology Co.,Ltd PU15-PRE FK1UC1R060115";
        };

        cuview_pix3_pro = default // {
          criteria = "DO NOT USE - RTK Pi-X3 Pro demoset-1"; #lol
        };

        asus_pa148 = {
          criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
          mode = "1920x1080";
          scale = 1.0;
        };
      in
      {
        "dual_xdr_uk650w" = {
          outputs = [
            (apple_pro_display_xdr // { position = "1920,0"; })
            lg_27uk650_w
          ];
        };
        "dual_pu15" = {
          outputs = [
            (innocn_pu15_pre // { position = "1920,0"; })
            lg_27uk650_w
          ];
        };
        "dual_cuview" = {
          outputs = [
            (cuview_pix3_pro // { position = "1920,0"; })
            lg_27uk650_w
          ];
        };
        "only_xdr" = {
          outputs = [
            apple_pro_display_xdr
          ];
        };
        "only_pa148" = {
          outputs = [
            asus_pa148
          ];
        };
        "fallback" = {
          outputs = [
            default
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
