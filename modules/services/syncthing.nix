{ config, lib, pkgs, ... }:

let
  cfg = config.services.syncthing;
in
{
  services.syncthing = {
    enable = true;
  };

  systemd.user.services.syncthing.Service = lib.mkIf cfg.enable {
    Slice = lib.mkDefault "background.slice";
  };
}
