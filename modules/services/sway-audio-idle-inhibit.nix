{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf;

  pkg = pkgs.unstable.sway-audio-idle-inhibit;
in
{
  systemd.user.services.sway-audio-idle-inhibit = {
    Service = {
      ExecStart = "${pkg}/bin/sway-audio-idle-inhibit";
      Restart = "on-failure";
    };

    Install = { WantedBy = [ "sway-session.target" ]; };
  };

  programs.waybar = {
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
