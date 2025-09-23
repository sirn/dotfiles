{ config, lib, pkgs, ... }:

let
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
    ${lib.getExe swwwPkg} query | ${lib.getExe pkgs.gawk} -F 'image: ' '{ print $2 }'
  '';
in
{
  home.packages = [ swwwPkg ];

  # Defining systemd unit directly instead of using service.swww since we need
  # to override xrgb value (due to Sway + integrated GPUs shenanigans).
  # See also: https://github.com/LGFae/swww/issues/233
  #
  # TODO: switch back to service.swww on home-manager >= 25.11 and use extraArgs
  systemd.user.services.swww = {
    Unit = {
      Description = "A Solution to your Wayland Wallpaper Woes";
      After = [ config.wayland.systemd.target ];
      PartOf = [ config.wayland.systemd.target ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Restart = "always";
      RestartSec = 10;
      ExecStart = "${lib.getExe' swwwPkg "swww-daemon"} ${lib.escapeShellArgs [
        "--format" "xrgb"
      ]}";
    };

    Install.WantedBy = [ config.wayland.systemd.target ];
  };


  systemd.user.services."swww-wallpaper" = {
    Unit = {
      Description = "Update wallpaper with swww";
      After = [ config.wayland.systemd.target ];
      PartOf = [ config.wayland.systemd.target ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${wallpaperScript}/bin/swww-wallpaper";
    };
  };

  systemd.user.timers."swww-wallpaper" = {
    Unit = {
      Description = "Rotate wallpaper hourly";
      PartOf = [ config.wayland.systemd.target ];
    };

    Timer = {
      OnBootSec = "30s";
      OnUnitActiveSec = "1h";
      Unit = "swww-wallpaper.service";
      Persistent = true;
    };

    Install.WantedBy = [ config.wayland.systemd.target "timers.target" ];
  };

  programs.swaylock.settings = lib.mkIf swaylockcfg.enable {
    image = "$(${lib.getExe getSwwwImage})";
  };
}
