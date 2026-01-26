{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;

  skillsDir = ../../var/agents/skills;
  instructionText = builtins.readFile ../../var/agents/instruction.md;

  isStdioServer = server: server ? command || server ? package;

  toClaudeCodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          type = "stdio";
          command = server.command or (lib.getExe server.package);
        } else {
          type = server.transport or "sse";
          url = server.url;
        })
      servers;
in
{
  programs.claude-code = {
    enable = true;
    package = pkgs.unstable.claude-code;

    memory.text = instructionText;
    mcpServers = toClaudeCodeMcpServers config.programs.mcp.servers;

    settings = {
      model = "opusplan";
      includeCoAuthoredBy = false;
      cleanupPeriodDays = 7;
      permissions = {
        allow = [
          "Read(**)"
          "Glob(*)"
          "Grep(*)"
          "Edit(**)"
          "Write(**)"
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
          "Bash(jj show:*)"
          "Bash(go test:*)"
          "Bash(go build:*)"
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
          "Read(**/.env)"
          "Read(**/.env.*)"
          "Read(**/*.env)"
          "Edit(**/.env)"
          "Edit(**/.env.*)"
          "Edit(**/*.env)"
          "Write(**/.env)"
          "Write(**/.env.*)"
          "Write(**/*.env)"
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

  home.file.".claude/skills".source = lib.mkIf cfg.enable skillsDir;
}
