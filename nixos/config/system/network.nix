{ config, lib, ... }:

{
  networking.hostId = lib.mkDefault (
    builtins.substring 0 8 (builtins.hashString "sha256" config.networking.hostName)
  );

  services.resolved = {
    fallbackDns = [ ];
  };

  networking = {
    useDHCP = false;
    useNetworkd = true;

    nftables = {
      enable = true;
    };

    firewall = {
      enable = true;
    };
  };

  systemd.network = {
    enable = true;

    wait-online = {
      enable = lib.mkDefault false;
    };
  };
}
