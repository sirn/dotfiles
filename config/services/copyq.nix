{ config, lib, pkgs, ... }:

let
  cfg = config.services.copyq;
in
{
  services.copyq = {
    enable = true;
  };

  systemd.user.services.copyq.Service = lib.mkIf cfg.enable {
    Slice = lib.mkDefault "app.slice";
  };
}
