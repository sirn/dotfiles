{ config, lib, ... }:

{
  networking.hostId = lib.mkDefault (
    builtins.substring 0 8 (builtins.hashString "sha256" config.networking.hostName)
  );

  services.resolved = {
    fallbackDns = [ ];
  };

  networking = {
    nftables = {
      enable = true;
    };

    firewall = {
      enable = true;
    };
  };
}
