{
  lib,
  config,
  pkgs,
  ...
}:

let
  swaycfg = config.wayland.windowManager.sway;

  swayidlecfg = config.services.swayidle;

  niricfg = config.programs.niri;

  noctaliaCfg = config.programs.noctalia;

  suspendOnBattery = pkgs.writeShellApplication {
    name = "suspend-on-battery";
    runtimeInputs = [ pkgs.systemd ];
    text = ''
      if [[ "$(systemd-ac-power --verbose)" == "no" ]]; then
        exec "$@"
      fi
    '';
  };
in
{
  services.swayidle = lib.mkIf swayidlecfg.enable {
    timeouts = [
      {
        timeout = 900;
        command = "${lib.getExe suspendOnBattery} ${config.systemd.user.systemctlPath} suspend";
      }
    ];
  };

  programs.noctalia = lib.mkIf noctaliaCfg.enable {
    settings = {
      idle = {
        behavior_order = [
          "lock"
          "screen-off"
          "lock-and-suspend"
        ];
        behavior = {
          "lock-and-suspend" = {
            action = "command";
            command = "${lib.getExe suspendOnBattery} ${lib.getExe noctaliaCfg.package} msg session lock-and-suspend";
            enabled = true;
            timeout = 900;
          };
        };
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    extraConfig = ''
      bindswitch --locked lid:on output eDP-1 disable
      bindswitch --locked lid:off output eDP-1 enable
    '';
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      switch-events = {
        "lid-open".action.spawn = [
          "${lib.getExe niricfg.package}"
          "msg"
          "output"
          "eDP-1"
          "on"
        ];
        "lid-close".action.spawn = [
          "${lib.getExe niricfg.package}"
          "msg"
          "output"
          "eDP-1"
          "off"
        ];
      };
    };
  };
}
