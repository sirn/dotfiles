{ config, pkgs, lib, ... }:

{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;

    socketActivation = {
      enable = true;
    };
  };

  systemd.user.services.emacs.Service = {
    Slice = lib.mkDefault "app.slice";
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
