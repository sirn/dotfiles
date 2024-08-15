{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;
in
{
  services.mako = {
    enable = isLinux;
  };

  wayexec.services.mako = {
    runScript = ''
      #!${pkgs.execline}/bin/execlineb
      fdmove -c 2 1
      ${pkgs.mako}/bin/mako
    '';
  };
}
