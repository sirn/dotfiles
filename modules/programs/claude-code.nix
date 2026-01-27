{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  agentsDir = ../../var/agents/agents;

  agentFiles = builtins.filter
    (name: lib.hasSuffix ".toml" name)
    (builtins.attrNames (builtins.readDir agentsDir));

  loadAgent = tomlFile:
    let
      name = lib.removeSuffix ".toml" tomlFile;
      agentConfig = builtins.fromTOML (builtins.readFile (agentsDir + "/${tomlFile}"));
      prompt = builtins.readFile (agentsDir + "/${name}.md");
    in
    {
      inherit name;
      value = agentConfig // { inherit prompt; };
    };

  agents = builtins.listToAttrs (map loadAgent agentFiles);

  mkClaudeCodeAgent = name: agent: ''
    ---
    name: ${name}
    description: ${agent.description}
    tools: ${lib.concatStringsSep ", " agent.claude-code.allowedTools}
    color: ${agent.claude-code.color}
    model: ${agent.claude-code.model}
    ---
    ${agent.prompt}
  '';

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

    agents = lib.mapAttrs mkClaudeCodeAgent agents;
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
