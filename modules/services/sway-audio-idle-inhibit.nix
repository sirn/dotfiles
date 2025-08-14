{ config, pkgs, lib, ... }:

let
  pkg = pkgs.unstable.sway-audio-idle-inhibit;
in
{
  systemd.user.services.sway-audio-idle-inhibit = {
    Service = {
      ExecStart = "${pkg}/bin/sway-audio-idle-inhibit";
      Restart = "on-failure";
    };

    Install = { WantedBy = [ config.wayland.systemd.target ]; };
  };

  programs.waybar = lib.mkIf config.programs.waybar.enable {
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
