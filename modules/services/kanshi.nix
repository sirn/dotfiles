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
        profile = {
          name = "only_3225qf";
          outputs = [
            {
              criteria = "Dell Inc. AW3225QF 13T4YZ3";
              mode = "3840x2160";
              position = "0,0";
              scale = 1.5;
            }
          ];
        };
      }
      {
        profile = {
          name = "only_pa148";
          outputs = [
            {
              criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
              mode = "1920x1080";
              scale = 1.5;
            }
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
