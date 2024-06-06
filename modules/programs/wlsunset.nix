{ config, pkgs, lib, ... }:

let
  inherit (lib) cli concatStringsSep mkForce mkIf mkOption types;

  cfg = config.services.wlsunset;

  args = cli.toGNUCommandLineShell { } {
    t = cfg.temperature.night;
    T = cfg.temperature.day;
    g = cfg.gamma;
    l = cfg.latitude;
    L = cfg.longitude;
    S = cfg.sunrise;
    s = cfg.sunset;
    o = cfg.output;
  };
in
{
  config = {
    services.wlsunset = {
      enable = config.machine.isNixOS;

      # wlsunset on stable is buggy
      package = pkgs.unstable.wlsunset;

      systemdTarget = "sway-session.target";

      latitude = 35.67;
      longitude = 139.77;
      temperature = {
        night = 5000;
      };
    };

    systemd.user.services = mkIf cfg.enable {
      wlsunset = {
        Service = mkForce {
          ExecStart = "${cfg.package}/bin/wlsunset ${args}";
        };
      };
    };

    # non-NixOS; assume no systemd
    wayexec.services =
      mkIf (!config.services.wlsunset.enable) {
        wlsunset = {
          runScript = ''
            #!${pkgs.execline}/bin/execlineb
            fdmove -c 2 1
            ${pkgs.unstable.wlsunset}/bin/wlsunset ${args}
          '';
        };
      };
  };
}
