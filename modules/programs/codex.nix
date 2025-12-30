{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;
in
{
  programs.codex = {
    enable = true;

    package = pkgs.unstable.codex;
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".codex/"
    ];
  };
}
