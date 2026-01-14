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
          "Write(*)"
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
          "Bash(tree:*)"
          "Bash(lstr:*)"
          "Read(**/*.env.example)"
          "Read(**/*.env.sample)"
          "Write(**/*.env.example)"
          "Write(**/*.env.sample)"
          "WebSearch"
          "WebFetch(domain:*)"
        ];
        deny = [
          "Bash(sudo:*)"
          "Bash(kill:*)"
          "Bash(systemctl:*)"
          "Bash(chown:*)"
          "Bash(sops:*)"
          "Bash(git push:*)"
          "Bash(jj git push:*)"
          "Read(**/.env*)"
          "Read(**/*.env)"
          "Read(**/*.env.*)"
          "Write(**/.env*)"
          "Write(**/*.env)"
          "Write(**/*.env.*)"
        ];
        ask = [
          "Bash(chmod:*)"
          "Bash(rm:*)"
          "Bash(git commit:*)"
          "Bash(jj git:*)"
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".claude/*.local.json"
      ".claude/*.local.md"
    ];
  };
}
