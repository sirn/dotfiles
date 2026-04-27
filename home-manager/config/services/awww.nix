{
  config,
  lib,
  pkgs,
  ...
}:

let
  awwwPkg = config.services.awww.package;

  wallpaperScript = pkgs.writeScriptBin "awww-wallpaper" ''
    #!${pkgs.python3}/bin/python3
    import os
    import random
    import subprocess
    import sys
    import time

    # Supported formats: jpeg, png, gif, pnm, tga, tiff, webp, bmp, farbfeld, avif, svg
    VALID_EXTS = {
        ".jpg", ".jpeg", ".png", ".gif", ".pnm", ".tga", ".tiff", ".webp",
        ".bmp", ".farbfeld", ".avif", ".svg"
    }

    wallpapers = os.path.expanduser("~/.local/wallpapers")
    if not os.path.isdir(wallpapers):
        sys.exit(0)

    candidates = [
        os.path.join(root, name)
        for root, _, files in os.walk(wallpapers)
        for name in files
        if os.path.splitext(name)[1].lower() in VALID_EXTS
    ]

    if not candidates:
        sys.exit(0)

    # Try up to 5 times
    for _ in range(5):
        choice = random.choice(candidates)
        try:
            subprocess.run(
                ["${lib.getExe awwwPkg}", "img", choice],
                check=True,
            )
            sys.exit(0)
        except subprocess.CalledProcessError:
            print(f"Failed to set wallpaper: {choice}", file=sys.stderr)
            candidates.remove(choice)
            if not candidates:
                break
            time.sleep(1)

    sys.exit(1)
  '';

  swaylockcfg = config.programs.swaylock;

  nixWallpaper = "${pkgs.nixos-artwork.wallpapers.nineish-catppuccin-mocha}/share/backgrounds/nixos/nix-wallpaper-nineish-catppuccin-mocha.png";

  getAwwwImage = pkgs.writeScriptBin "get-awww-image" ''
    #!${pkgs.runtimeShell}
    img=$(${lib.getExe awwwPkg} query -j | ${lib.getExe pkgs.jaq} -r '.[][].displaying.image // empty' | ${lib.getExe' pkgs.coreutils "tail"} -n1)
    if [ -n "$img" ] && [ -f "$img" ]; then
      printf '%s' "$img"
    else
      printf '%s' "${nixWallpaper}"
    fi
  '';
in
{
  services.awww = {
    enable = true;
    package = pkgs.unstable.awww;
  };

  # The awww-daemon service is provided by the upstream home-manager module.
  systemd.user.services.awww.Service.Slice = lib.mkDefault "app.slice";

  systemd.user.services."awww-restore" = {
    Unit = {
      Description = "Restore wallpaper with awww";
      After = [
        config.wayland.systemd.target
        "awww.service"
      ];
      PartOf = [ config.wayland.systemd.target ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Type = "oneshot";
      Slice = lib.mkDefault "app.slice";
      ExecStart = "${lib.getExe awwwPkg} restore";
    };

    Install.WantedBy = [ config.wayland.systemd.target ];
  };

  systemd.user.services."awww-wallpaper" = {
    Unit = {
      Description = "Update wallpaper with awww";
      After = [
        config.wayland.systemd.target
        "awww.service"
      ];
      PartOf = [ config.wayland.systemd.target ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Type = "oneshot";
      Slice = lib.mkDefault "app.slice";
      ExecStart = "-${wallpaperScript}/bin/awww-wallpaper";
    };
  };

  systemd.user.timers."awww-wallpaper" = {
    Unit = {
      Description = "Rotate wallpaper hourly";
      PartOf = [ config.wayland.systemd.target ];
    };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "5min";
      Unit = "awww-wallpaper.service";
    };

    Install.WantedBy = [
      config.wayland.systemd.target
      "timers.target"
    ];
  };

  programs.swaylock.settings = lib.mkIf swaylockcfg.enable {
    image = "$(${lib.getExe getAwwwImage})";
  };
}
