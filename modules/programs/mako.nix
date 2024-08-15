{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  services.mako = {
    enable = true;
  };

  wayexec.services.mako = {
    runScript = ''
      #!${pkgs.execline}/bin/execlineb
      fdmove -c 2 1
      ${pkgs.mako}/bin/mako
    '';
  };
}
