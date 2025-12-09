{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  fuzzelcfg = config.programs.fuzzel;

  # Slice assignment based on:
  # https://github.com/Vladimir-csp/uwsm/

  appGraphicalSlice = "app-graphical.slice";

  backgroundGraphicalSlice = "background-graphical.slice";

  uwsmFinalize = pkgs.writeShellScript "uwsm-finalize" ''
    if command -v uwsm >/dev/null; then
      exec uwsm finalize
    fi
  '';

  wrapLauncher = x:
    let
      cmd =
        if lib.isDerivation x
        then lib.getExe x
        else x;
    in
    pkgs.writeShellScript "launcher" ''
      if command -v uwsm >/dev/null; then
        exec uwsm app -- ${cmd}
      else
        exec ${cmd}
      fi
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

    emacs.Service = lib.mkIf (config.systemd.user.services ? emacs) {
      Slice = appGraphicalSlice;
    };

    languagetool.Service = lib.mkIf (config.systemd.user.services ? languagetool) {
      Slice = backgroundGraphicalSlice;
    };

    swww.Service = lib.mkIf config.services.swww.enable {
      Slice = backgroundGraphicalSlice;
    };

    "swww-wallpaper".Service = lib.mkIf config.services.swww.enable {
      Slice = backgroundGraphicalSlice;
    };

    wlsunset.Service = lib.mkIf config.services.wlsunset.enable {
      Slice = backgroundGraphicalSlice;
    };

    kanshi.Service = lib.mkIf config.services.kanshi.enable {
      Slice = backgroundGraphicalSlice;
    };

    swayidle.Service = lib.mkIf config.services.swayidle.enable {
      Slice = backgroundGraphicalSlice;
    };

    sway-audio-idle-inhibit.Service = lib.mkIf (config.systemd.user.services ? sway-audio-idle-inhibit) {
      Slice = backgroundGraphicalSlice;
    };
  };
}
