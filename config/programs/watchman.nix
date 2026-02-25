{ lib, config, pkgs, ... }:

let
  jjcfg = config.programs.jujutsu;
in
{
  home.packages = [
    pkgs.watchman
  ];

  programs.jujutsu = lib.mkIf jjcfg.enable {
    settings = {
      fsmonitor = {
        backend = "watchman";
        watchman = {
          register-snapshot-trigger = true;
        };
      };
    };
  };
}
