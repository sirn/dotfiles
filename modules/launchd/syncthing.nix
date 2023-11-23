{ config, pkgs, ... }:

{
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
