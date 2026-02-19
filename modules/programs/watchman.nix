{ lib, config, pkgs, ... }:

let
  jjcfg = config.programs.jujutsu;
in
{
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.watchman
  ];

  programs.jujutsu = lib.mkIf (jjcfg.enable && pkgs.stdenv.isLinux) {
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
