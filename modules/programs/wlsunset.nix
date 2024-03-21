{ config, pkgs, lib, ... }:

let
  inherit (lib) concatStringsSep mkForce mkIf mkOption types;

  cfg = config.services.wlsunset;

  args = [
    "-t ${toString cfg.temperature.night}"
    "-T ${toString cfg.temperature.day}"
    "-g ${cfg.gamma}"
  ] ++ (if cfg.latitude != "" && config.longitude != "" then [
    "-l ${cfg.latitude}"
    "-L ${cfg.longitude}"
  ] else [ ]) ++ (if cfg.sunrise != "" && cfg.sunset != "" then [
    "-S ${cfg.sunrise}"
    "-s ${cfg.sunset}"
  ] else [ ]);
in
{
  # Hack to support sunrise and sunset options from unstable.
  options.services.wlsunset = {
    sunrise = mkOption {
      type = types.str;
      description = ''
        A fixed time for sunrise.
      '';
    };

    sunset = mkOption {
      type = types.str;
      description = ''
        A fixed time for sunset.
      '';
    };
  };

  config =
    mkIf config.desktop.enable {
      services.wlsunset = {
        enable = config.machine.isNixOS;

        # wlsunset on stable is buggy
        package = pkgs.unstable.wlsunset;

        systemdTarget = "sway-session.target";

        latitude = "";
        longitude = "";
        sunrise = "7:00";
        sunset = "21:00";
      };

      systemd.user.services = mkIf cfg.enable {
        wlsunset = {
          Service = mkForce {
            ExecStart = "${cfg.package}/bin/wlsunset ${concatStringsSep " " args}";
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
              ${pkgs.unstable.wlsunset}/bin/wlsunset ${concatStringsSep " " args}
            '';
          };
        };
    };
}
