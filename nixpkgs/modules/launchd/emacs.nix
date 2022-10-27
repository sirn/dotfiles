{ config, pkgs, ... }:

{
  launchd.agents.emacs = {
    enable = true;
    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        "${config.programs.emacs.package}/bin/emacs --fg-daemon --chdir=$HOME"
      ];
    };
  };
}
