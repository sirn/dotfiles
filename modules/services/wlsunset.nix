{ lib, ... }:

{
  services.wlsunset = {
    enable = true;

    systemdTarget = "sway-session.target";

    latitude = lib.mkDefault 35.67;
    longitude = lib.mkDefault 139.77;

    temperature = {
      night = 4500;
    };
  };
}
