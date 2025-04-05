{ config, pkgs, lib, ... }:

{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;

    socketActivation = {
      enable = true;
    };
  };

  launchd.agents.emacs = {
    enable = true;
    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        "${config.programs.emacs.finalPackage}/bin/emacs --fg-daemon --chdir=$HOME"
      ];
    };
  };
}
