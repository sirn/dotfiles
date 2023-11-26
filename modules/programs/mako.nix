{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.desktop.enable {
  services.mako = {
    enable = config.machine.isNixOS;
  };

  # non-NixOS; assume no systemd
  wayland.windowManager.sway =
    mkIf (!config.services.mako.enable) {
      config = {
        startup = [
          {
            command = "${pkgs.mako}/bin/mako";
          }
        ];
      };
    };
}
