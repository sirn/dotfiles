{ lib, ... }:

{
  imports = [ ./base.nix ];

  networking = {
    useNetworkd = lib.mkForce false;
    useDHCP = lib.mkForce false;

    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };
}
