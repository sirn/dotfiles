{ lib, pkgs, ... }:

{
  services.copyq = {
    enable = true;
  };

  systemd.user.services.copyq.Service = {
    Slice = lib.mkDefault "app.slice";
  };
}
