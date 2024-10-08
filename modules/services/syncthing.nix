{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux isDarwin;
  inherit (lib) mkIf;
in
{
  services.syncthing = {
    enable = isLinux;
  };

  runit.services.syncthing = {
    runScript = ''
      #!${pkgs.execline}/bin/execlineb
      emptyenv -p
      export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
      export HOME ${homeDirectory}
      export XDG_CONFIG_HOME ${homeDirectory}/.config
      export XDG_DATA_HOME ${homeDirectory}/.local/share

      fdmove -c 2 1
      ${pkgs.syncthing}/bin/syncthing serve --no-browser --no-upgrade
    '';
  };

  launchd.agents.syncthing = {
    enable = true;

    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "${pkgs.syncthing}/bin/syncthing"
        "--no-browser"
        "--no-upgrade"
      ];
    };
  };
}
