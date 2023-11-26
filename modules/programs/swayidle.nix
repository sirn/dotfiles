{ config, lib, pkgs, ... }:

let
  inherit (lib) concatMap concatStringsSep escapeShellArg mkIf optionals;

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
mkIf config.desktop.enable {
  services.swayidle = {
    enable = config.machine.isNixOS;

    timeouts = [
      {
        timeout = 600;
        command = "${swaymsgBin} \"output * dpms off\"";
        resumeCommand = "${swaymsgBin} \"output * dpms on\"";
      }
    ];
  };

  # non-NixOS; assume no systemd
  wayland.windowManager.sway =
    mkIf (!config.services.swayidle.enable) {
      config = {
        startup = [
          {
            always = true;
            command = "${pkgs.writeScriptBin "start-swayidle" ''
              #!${pkgs.bash}/bin/bash
              pkill -Af swayidle

              run_and_disown() {
                "$@" &
                sleep 0.5
                disown
              }

              run_and_disown ${config.services.swayidle.package}/bin/swayidle -w ${concatStringsSep " " args}
            ''}/bin/start-swayidle";
          }
        ];
      };
    };
}
