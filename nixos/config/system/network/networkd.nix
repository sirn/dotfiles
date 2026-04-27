{ lib, ... }:

{
  imports = [ ./base.nix ];

  networking = {
    useDHCP = false;
    useNetworkd = true;
  };

  systemd.network = {
    enable = true;

    wait-online = {
      enable = lib.mkDefault false;
    };
  };
}
