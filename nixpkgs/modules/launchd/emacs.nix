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
        "emacs --fg-daemon --chdir=$HOME"
      ];
    };
  };
}
