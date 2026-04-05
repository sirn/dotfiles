{
  networking = {
    wireless = {
      iwd = {
        enable = true;
      };
    };
  };

  systemd.network.networks = {
    "wlan0" = {
      matchConfig.Name = "wlan0";
      networkConfig = {
        IPv6AcceptRA = true;
        DHCP = "ipv4";
      };
    };
  };
}
