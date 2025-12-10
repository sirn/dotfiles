{ config, lib, ... }:

{
  services.wlsunset = {
    enable = true;

    systemdTarget = config.wayland.systemd.target;

    latitude = lib.mkDefault 35.67;
    longitude = lib.mkDefault 139.77;

    temperature = {
      night = 4500;
    };
  };

  systemd.user.services.wlsunset.Service = {
    Slice = lib.mkDefault "session.slice";
  };
}
