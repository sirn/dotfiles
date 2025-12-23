{ config, lib, pkgs, ... }:

let
  cfg = config.services.swww;

  swwwPkg = pkgs.swww;

  wallpaperScript = pkgs.writeScriptBin "swww-wallpaper" ''
    #!${pkgs.python3}/bin/python3
    import os
    import random
    import subprocess
    import sys

    wallpapers = os.path.expanduser("~/.local/wallpapers")
    if not os.path.isdir(wallpapers):
        sys.exit(0)

    candidates = [
        os.path.join(root, name)
        for root, _, files in os.walk(wallpapers)
        for name in files
    ]
    if not candidates:
        sys.exit(0)

    subprocess.run(
        ["${lib.getExe swwwPkg}", "img", random.choice(candidates)],
        check=True,
    )
  '';

  swaylockcfg = config.programs.swaylock;

  getSwwwImage = pkgs.writeScriptBin "get-swww-image" ''
    #!${pkgs.runtimeShell}
    ${lib.getExe swwwPkg} query | ${lib.getExe pkgs.gawk} -F 'image: ' '{ print $2 }' | ${lib.getExe' pkgs.coreutils "tail"} -n1
  '';
in
{
  services.swww = {
    enable = true;
    extraArgs = [
      "--format"
      "xrgb"
    ];
  };

  systemd.user.services."swww-wallpaper" = lib.mkIf cfg.enable {
    Unit = {
      Description = "Update wallpaper with swww";
      After = [ config.wayland.systemd.target "swww.service" ];
      PartOf = [ config.wayland.systemd.target ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Type = "oneshot";
      Slice = lib.mkDefault "app.slice";
      ExecStart = "-${wallpaperScript}/bin/swww-wallpaper";
    };
  };

  systemd.user.timers."swww-wallpaper" = lib.mkIf cfg.enable {
    Unit = {
      Description = "Rotate wallpaper hourly";
      PartOf = [ config.wayland.systemd.target ];
    };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "5min";
      Unit = "swww-wallpaper.service";
      Persistent = true;
    };

    Install.WantedBy = [ config.wayland.systemd.target "timers.target" ];
  };

  programs.swaylock.settings = lib.mkIf swaylockcfg.enable {
    image = "$(${lib.getExe getSwwwImage})";
  };
}
