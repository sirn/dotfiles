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

    systemdTarget = "sway-session.target";

    timeouts = [
      {
        timeout = 300;
        command = "${swaymsgBin} \"output * dpms off\"";
        resumeCommand = "${swaymsgBin} \"output * dpms on\"";
      }
    ];
  };

  wayland.windowManager.sway = {
    config = {
      keybindings = {
        "${swaycfg.modifier}+Ctrl+Shift+L" = "exec pkill -USR1 -f ${config.services.swayidle.package}/bin/swayidle";
      };
    };
  };
}
