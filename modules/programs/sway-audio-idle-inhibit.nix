{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf;

  pkg = pkgs.unstable.sway-audio-idle-inhibit;
in
mkIf config.desktop.enable {
  systemd.user.services =
    mkIf config.machine.isNixOS {
      sway-audio-idle-inhibit = {
        Service = {
          ExecStart = "${pkg}/bin/sway-audio-idle-inhibit";
        };
      };
    };

  wayexec.services =
    mkIf (!config.machine.isNixOS) {
      sway-audio-idle-inhibit = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          ${pkg}/bin/sway-audio-idle-inhibit
        '';
      };
    };
}
