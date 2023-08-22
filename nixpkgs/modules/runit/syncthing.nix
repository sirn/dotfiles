{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username;

  homeDir = config.home.homeDirectory;
in
{
  runit.services = {
    syncthing = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDir}
        export XDG_CONFIG_HOME ${homeDir}/.config
        export XDG_DATA_HOME ${homeDir}/.local/share

        fdmove -c 2 1
        ${pkgs.syncthing}/bin/syncthing serve --no-browser
      '';
    };
  };
}
