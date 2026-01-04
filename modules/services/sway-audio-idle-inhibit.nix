{ config, pkgs, lib, ... }:

let
  pkg = pkgs.sway-audio-idle-inhibit;

  waybarcfg = config.programs.waybar;
in
{
  systemd.user.services.sway-audio-idle-inhibit = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      After = [ config.wayland.systemd.target ];
      PartOf = [ config.wayland.systemd.target ];
    };

    Service = {
      ExecStart = "${pkg}/bin/sway-audio-idle-inhibit";
      Slice = lib.mkDefault "app.slice";
      Restart = "on-failure";
    };

    Install = { WantedBy = [ config.wayland.systemd.target ]; };
  };

  programs.waybar = lib.mkIf waybarcfg.enable {
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
