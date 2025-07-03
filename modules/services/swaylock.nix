{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway.config;

  swaylockBin =
    if config.programs.swaylock.enable
    then "${config.programs.swaylock.package}/bin/swaylock"
    else "/usr/bin/swaylock"; # no relative path here due to systemd unit setting PATH=
in
{
  programs.swaylock = {
    # swaylock needs to access PAM, so we must use the system package on non-NixOS
    enable = config.machine.isNixOS;

    settings =
      let
        bgSplit =
          builtins.match
            "^(.+)[[:space:]]+(stretch|fill|fit|center|tile).*"
            swaycfg.output."*".bg;
      in
      {
        daemonize = true;
        image = lib.elemAt bgSplit 0;
        scaling = lib.elemAt bgSplit 1;
      };
  };

  services.swayidle = {
    timeouts = [
      {
        timeout = 300;
        command = "${swaylockBin}";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${swaylockBin}";
      }
    ];
  };

  # Copied from home-manager/modules/programs/swaylock.nix
  xdg.configFile = lib.mkIf (!config.programs.swaylock.enable) {
    "swaylock/config" = {
      text = lib.concatStrings (lib.mapAttrsToList
        (n: v:
          if v == false then ""
          else (if v == true then n else n + "=" + builtins.toString v) + "\n")
        config.programs.swaylock.settings);
    };
  };
}
