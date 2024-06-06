{ config, lib, pkgs, ... }:

let
  inherit (lib) concatStrings elemAt mapAttrsToList mkIf;

  swaycfg = config.wayland.windowManager.sway.config;

  swaylockBin =
    if config.programs.swaylock.enable
    then "${config.programs.swaylock.package}/bin/swaylock"
    else "swaylock";
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
        image = elemAt bgSplit 0;
        scaling = elemAt bgSplit 1;
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
  xdg.configFile = mkIf (!config.programs.swaylock.enable) {
    "swaylock/config" = {
      text = concatStrings (mapAttrsToList
        (n: v:
          if v == false then ""
          else (if v == true then n else n + "=" + builtins.toString v) + "\n")
        config.programs.swaylock.settings);
    };
  };
}
