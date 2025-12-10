{ config, lib, pkgs, ... }:

{
  services.mako = {
    enable = true;
  };

  systemd.user.services.mako.Service = {
    Slice = lib.mkDefault "session.slice";
  };
}
