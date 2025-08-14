{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway.config;

  swaymsgBin =
    if config.wayland.windowManager.sway.package != null
    then "${config.wayland.windowManager.sway.package}/bin/swaymsg"
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

  wayland.windowManager.sway = lib.mkIf config.wayland.windowManager.sway.enable {
    config = {
      keybindings = {
        "${swaycfg.modifier}+Ctrl+Shift+L" = "exec pkill -USR1 -f ${lib.getExe config.services.swayidle.package}";
      };
    };
  };

  programs.niri = lib.mkIf config.programs.niri.enable {
    settings = {
      binds = {
        "Mod+Alt+L".action.spawn = [
          "pkill"
          "-USR1"
          "-f"
          "${lib.getExe config.services.swayidle.package}"
        ];
      };
    };
  };
}
