{ config, lib, pkgs, ... }:

let
  cfg = config.services.kanshi;

  swaycfg = config.wayland.windowManager.sway;

  swaymsgBin =
    if swaycfg.package != null
    then "${swaycfg.package}/bin/swaymsg"
    else "swaymsg";

  niricfg = config.programs.niri;
in
{
  services.kanshi = {
    enable = true;

    settings =
      let
        default = {
          status = lib.mkDefault "enable";
          criteria = "*";
          mode = "3840x2160";
          position = "0,0";
          scale = 2.0;
        };

        apple_pro_display_xdr = default // {
          criteria = "Apple Computer Inc ProDisplayXDR 0x00001F07";
          mode = "6016x3384";
          scale = 2.0;
        };

        dell_aw3225qf = default // {
          criteria = "Dell Inc. AW3225QF 13T4YZ3";
          scale = 1.5;
        };

        asus_pa148 = {
          criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
          mode = "1920x1080";
          scale = 1.5;
        };
      in
      [
        {
          profile = {
            name = "only_3225qf";
            outputs = [
              dell_aw3225qf
            ];
          };
        }
        {
          profile = {
            name = "only_xdr";
            outputs = [
              apple_pro_display_xdr
            ];
          };
        }
        {
          profile = {
            name = "only_pa148";
            outputs = [
              asus_pa148
            ];
          };
        }
      ];
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      keybindings = {
        "${swaycfg.config.modifier}+Ctrl+Shift+F10" = "exec pkill -INT -f ${lib.getExe cfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+Alt+F10".action.spawn = [
          "pkill"
          "-INT"
          "-f"
          "${lib.getExe cfg.package}"
        ];
      };
    };
  };
}
