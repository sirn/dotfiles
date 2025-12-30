{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;
in
{
  programs.claude-code = {
    enable = true;

    package = pkgs.unstable.claude-code;

    settings = {
      includeCoAuthoredBy = false;
      cleanupPeriodDays = 7;
      permissions = {
        allow = [
          "Bash(jj diff:*)"
          "Bash(jj status:*)"
          "Bash(jj log:*)"
        ];
        deny = [
          "Bash(jj git:*)"
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".claude/"
    ];
  };
}
