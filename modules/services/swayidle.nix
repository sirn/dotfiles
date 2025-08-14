{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway.config;

  swaypkg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  swayidlecfg = config.services.swayidle;

  swaymsgBin =
    if swaypkg.package != null
    then "${swaypkg.package}/bin/swaymsg"
    else "/usr/bin/swaymsg";
in
{
  services.swayidle = {
    enable = true;

    timeouts = [
      {
        timeout = 210;
        command = "${swaymsgBin} \"output * dpms off\"";
        resumeCommand = "${swaymsgBin} \"output * dpms on\"";
      }
    ];
  };

  wayland.windowManager.sway = lib.mkIf swaypkg.enable {
    config = {
      keybindings = {
        "${swaycfg.modifier}+Ctrl+Shift+L" = "exec pkill -USR1 -f ${lib.getExe swayidlecfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+Alt+L".action.spawn = [
          "pkill"
          "-USR1"
          "-f"
          "${lib.getExe swayidlecfg.package}"
        ];
      };
    };
  };
}
