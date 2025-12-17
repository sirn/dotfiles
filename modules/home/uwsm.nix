{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

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
      exec ${lib.getExe pkgs.app2unit} -- ${cmd}
    '';
in
{
  machine.wrapLauncher = wrapLauncher;

  home.packages = with pkgs; [
    app2unit
  ];

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    systemd = {
      enable = lib.mkForce false;
    };

    config = {
      startup = [
        { command = "${uwsmFinalize}"; }
      ];
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    systemd = {
      enable = lib.mkForce false;
    };
  };

  programs.fuzzel.settings = lib.mkIf fuzzelcfg.enable {
    main = {
      launch-prefix = "${lib.getExe pkgs.app2unit} --fuzzel-compat --";
    };
  };

  systemd.user.services = {
    waybar.Service = lib.mkIf config.programs.waybar.enable {
      Slice = appGraphicalSlice;
    };

    swww.Service = lib.mkIf config.services.swww.enable {
      Slice = appGraphicalSlice;
    };

    "swww-wallpaper".Service = lib.mkIf config.services.swww.enable {
      Slice = appGraphicalSlice;
    };

    copyq.Service = lib.mkIf config.services.copyq.enable {
      Slice = appGraphicalSlice;
    };

    wlsunset.Service = lib.mkIf config.services.wlsunset.enable {
      Slice = appGraphicalSlice;
    };

    kanshi.Service = lib.mkIf config.services.kanshi.enable {
      Slice = appGraphicalSlice;
    };

    swayidle.Service = lib.mkIf config.services.swayidle.enable {
      Slice = appGraphicalSlice;
    };

    sway-audio-idle-inhibit.Service = lib.mkIf (config.systemd.user.services ? sway-audio-idle-inhibit) {
      Slice = appGraphicalSlice;
    };
  };
}
