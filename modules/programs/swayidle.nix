{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) concatMap concatStringsSep escapeShellArg mkIf optionals;

  swaycfg = config.wayland.windowManager.sway.config;

  swaymsgBin =
    if config.wayland.windowManager.sway.package != null
    then "${config.wayland.windowManager.sway.package}/bin/swaymsg"
    else "swaymsg";

  # Copied from home-manager/modules/services/swayidle.nix
  cfg = config.services.swayidle;

  mkTimeout = t:
    [ "timeout" (toString t.timeout) (escapeShellArg t.command) ]
    ++ optionals (t.resumeCommand != null) [
      "resume"
      (escapeShellArg t.resumeCommand)
    ];

  mkEvent = e: [ e.event (escapeShellArg e.command) ];

  args = cfg.extraArgs ++ (concatMap mkTimeout cfg.timeouts)
    ++ (concatMap mkEvent cfg.events);
in
{
  services.swayidle = {
    enable = isLinux;

    systemdTarget = "sway-session.target";

    timeouts = [
      {
        timeout = 600;
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

  wayexec.services.swayidle = {
    # bash is used here due to shell escaping shenanigans
    runScript = ''
      #!${pkgs.bash}/bin/bash
      exec 2>&1
      exec ${config.services.swayidle.package}/bin/swayidle -w ${concatStringsSep " " args}
    '';
  };
}
