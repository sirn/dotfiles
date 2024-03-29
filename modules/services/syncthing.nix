{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux isDarwin;
  inherit (lib) mkIf;
in
{
  services = mkIf config.machine.isNixOS {
    syncthing = {
      enable = true;
    };
  };

  runit.services = mkIf (isLinux && config.runit.enable) {
    syncthing = {
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
  };

  launchd.agents.syncthing = mkIf isDarwin {
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
