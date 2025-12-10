{ config, lib, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
  };

  systemd.user.services.syncthing.Service = {
    Slice = lib.mkDefault "background.slice";
  };
}
