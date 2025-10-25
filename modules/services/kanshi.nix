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

    settings = [
      {
        output = {
          alias = "aw3225qf";
          adaptiveSync = true;
          criteria = "Dell Inc. AW3225QF 13T4YZ3";
          mode = "3840x2160@240Hz";
          position = "0,0";
          scale = 1.5;
        };
      }
      {
        output = {
          alias = "pa148";
          criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
          mode = "1920x1080@60Hz";
          scale = 1.5;
        };
      }
      {
        profile = {
          name = "only_aw3225qf";
          outputs = [
            { criteria = "$aw3225qf"; }
          ];
        };
      }
      {
        profile = {
          name = "only_pa148";
          outputs = [
            { criteria = "$pa148"; }
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
