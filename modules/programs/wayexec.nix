{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf (!config.wayland.windowManager.sway.systemd.enable) {
  wayexec = {
    enable = true;
  };

  wayland.windowManager.sway = {
    config = {
      startup = [
        {
          command = ''
            ${pkgs.writeScriptBin "start-wayexec" ''
              #!${pkgs.bash}/bin/bash
              exec runsvdir "${config.wayexec.serviceDir}" 'log: ...........................................................................................................................................................................................................................................................................................................................................................................................................'
            ''}/bin/start-wayexec
          '';
        }
      ];
    };
  };
}
