{ config, lib, ... }:

{
  networking.wireless.iwd = lib.mkIf config.systemd.network.enable { enable = true; };

  systemd.network.networks = lib.mkIf config.systemd.network.enable {
    "wlan0" = {
      matchConfig.Name = "wlan0";
      networkConfig = {
        IPv6AcceptRA = true;
        DHCP = "ipv4";
      };
    };
  };
}
