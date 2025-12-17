{ lib, config, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  swayidlecfg = config.services.swayidle;

  niricfg = config.programs.niri;
in
{
  services.swayidle = lib.mkIf swayidlecfg.enable {
    timeouts = [
      {
        timeout = 300;
        command = "${config.systemd.user.systemctlPath} suspend";
      }
    ];
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
        "lid-open".action.spawn = [ "${lib.getExe niricfg.package}" "msg" "output" "eDP-1" "on" ];
        "lid-close".action.spawn = [ "${lib.getExe niricfg.package}" "msg" "output" "eDP-1" "off" ];
      };
    };
  };
}
