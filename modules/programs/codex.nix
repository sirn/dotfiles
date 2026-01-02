{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;
in
{
  programs.codex = {
    enable = true;

    package = pkgs.unstable.codex;

    settings = {
      projects = lib.mkMerge [
        {
          "${config.home.homeDirectory}/.dotfiles" = {
            trust_level = "untrusted";
          };
        }
        (lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable) {
          "/etc/nixos" = {
            trust_level = "untrusted";
          };
        })
      ];
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".codex/"
    ];
  };
}
