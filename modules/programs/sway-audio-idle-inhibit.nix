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
          Restart = "on-failure";
        };

        Install = { WantedBy = [ "sway-session.target" ]; };
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

  programs.waybar =
    mkIf config.programs.waybar.enable {
      settings = {
        mainBar = {
          modules-right = [ "custom/audio_idle_inhibitor" ];

          "custom/audio_idle_inhibitor" = {
            format = "{icon}";
            exec = "${pkg}/bin/sway-audio-idle-inhibit --dry-print-both-waybar";
            return-type = "json";
            format-icons = {
              output = "";
              input = "";
              output-input = "";
              none = "";
            };
          };
        };
      };
    };
}
