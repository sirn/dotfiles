{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  fuzzelcfg = config.programs.fuzzel;

  # Slice assignment based on:
  # https://github.com/Vladimir-csp/uwsm/

  appGraphicalSlice = "app-graphical.slice";

  backgroundGraphicalSlice = "background-graphical.slice";

  sessionGraphicalSlice = "session-graphical.slice";

  uwsmFinalize = pkgs.writeShellScript "uwsm-finalize" ''
    exec uwsm finalize
  '';

  wrapLauncher = x:
    let
      name =
        if lib.isDerivation x
        then x.pname or x.name
        else baseNameOf x;

      cmd =
        if lib.isDerivation x
        then lib.getExe x
        else x;
    in
    pkgs.writeShellScript "uwsm-${name}" ''
      exec uwsm app -- ${cmd}
    '';
in
{
  machine.wrapLauncher = wrapLauncher;
  wayland.windowManager.sway.config = lib.mkIf swaycfg.enable {
    startup = [
      { command = "${uwsmFinalize}"; }
    ];
  };

  programs.fuzzel.settings = lib.mkIf fuzzelcfg.enable {
    main = {
      launch-prefix = "uwsm app --";
    };
  };

  systemd.user.services = {
    waybar.Service = lib.mkIf config.programs.waybar.enable {
      Slice = appGraphicalSlice;
    };

    swww.Service = lib.mkIf config.services.swww.enable {
      Slice = sessionGraphicalSlice;
    };

    "swww-wallpaper".Service = lib.mkIf config.services.swww.enable {
      Slice = sessionGraphicalSlice;
    };

    copyq.Service = lib.mkIf config.services.copyq.enable {
      Slice = sessionGraphicalSlice;
    };

    wlsunset.Service = lib.mkIf config.services.wlsunset.enable {
      Slice = sessionGraphicalSlice;
    };

    kanshi.Service = lib.mkIf config.services.kanshi.enable {
      Slice = sessionGraphicalSlice;
    };

    swayidle.Service = lib.mkIf config.services.swayidle.enable {
      Slice = sessionGraphicalSlice;
    };

    sway-audio-idle-inhibit.Service = lib.mkIf (config.systemd.user.services ? sway-audio-idle-inhibit) {
      Slice = sessionGraphicalSlice;
    };
  };
}
