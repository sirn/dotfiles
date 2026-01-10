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
          "Read(*)"
          "Glob(*)"
          "Grep(*)"
          "Bash(cat:*)"
          "Bash(find:*)"
          "Bash(fd:*)"
          "Bash(grep:*)"
          "Bash(rg:*)"
          "Bash(ls:*)"
          "Bash(curl:*)"
          "Bash(wget:*)"
          "Bash(git status:*)"
          "Bash(git diff:*)"
          "Bash(git log:*)"
          "Bash(git branch:*)"
          "Bash(jj status:*)"
          "Bash(jj diff:*)"
          "Bash(jj log:*)"
        ];
        deny = [
          "Bash(sudo:*)"
          "Bash(kill:*)"
          "Bash(systemctl:*)"
          "Bash(chmod:*)"
          "Bash(chown:*)"
          "Bash(sops:*)"
        ];
        ask = [
          "Bash(rm:*)"
          "Bash(git push:*)"
          "Bash(git commit:*)"
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
