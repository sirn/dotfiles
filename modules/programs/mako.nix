{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  services.mako = {
    enable = config.machine.isNixOS;
  };

  # non-NixOS; assume no systemd
  wayexec.services =
    mkIf (!config.services.mako.enable) {
      mako = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          ${pkgs.mako}/bin/mako
        '';
      };
    };
}
