{ config, pkgs, lib, ... }:

let
  cfg = config.services.emacs;
in
{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;

    socketActivation = {
      enable = true;
    };
  };

  systemd.user.services.emacs.Service = lib.mkIf cfg.enable {
    Slice = lib.mkDefault "app.slice";
    Environment = [
      "COLORTERM=truecolor"
    ];
  };

  launchd.agents.emacs = lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "${config.programs.emacs.finalPackage}/bin/emacs"
        "--fg-daemon"
        "--chdir=${config.home.homeDirectory}"
      ];
      EnvironmentVariables = {
        COLORTERM = "truecolor";
        TERMINFO_DIRS = "${config.home.homeDirectory}/.nix-profile/share/terminfo";
      };
    };
  };
}
