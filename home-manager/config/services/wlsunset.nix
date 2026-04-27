{ config, lib, ... }:

let
  cfg = config.services.wlsunset;
in
{
  services.wlsunset = {
    enable = true;

    systemdTarget = config.wayland.systemd.target;

    sunrise = lib.mkDefault "06:30";
    sunset = lib.mkDefault "18:30";

    temperature = {
      night = 4500;
    };
  };

  systemd.user.services.wlsunset = lib.mkIf cfg.enable {
    Unit.ConditionEnvironment = "WAYLAND_DISPLAY";
    Service.Slice = lib.mkDefault "app.slice";
  };
}
