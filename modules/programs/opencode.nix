{ config, lib, pkgs, ... }:

let
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

  # Filter agents that have opencode configuration
  opencodeAgents = lib.filterAttrs (name: agent: agent ? opencode) agents;

  validateOpencodeAgent = name: agent:
    let
      valid =
        if !(agent ? description) then throw "Agent ${name}: missing 'description'"
        else agent;
    in valid;

  validOpencodeAgents = lib.mapAttrs validateOpencodeAgent opencodeAgents;

  mkOpencodeAgent = name: agent:
    let
      isPrimary = (agent.opencode.primary or false);
      modelLine = lib.optionalString (isPrimary && agent.opencode ? model) "model: ${agent.opencode.model}\n";
      modeVal = if !isPrimary then "subagent" else (agent.opencode.mode or "primary");
      modeLine = lib.optionalString (modeVal != "") "mode: ${modeVal}\n";
      permissionLine = lib.optionalString (agent.opencode ? permission) "permission: ${builtins.toJSON agent.opencode.permission}\n";
    in
    ''
      ---
      description: ${agent.description}
      ${modelLine}${modeLine}${permissionLine}---
      ${agent.prompt}
    '';

  isStdioServer = server: server ? command || server ? package;

  toOpencodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = [ (server.command or (lib.getExe server.package)) ];
          type = "local";
          enabled = true;
        } else {
          url = server.url;
          type = "remote";
          enabled = true;
        })
      servers;
in
{
  programs.opencode = {
    enable = true;

    package = (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${lib.getExe pkgs.unstable.opencode}" "$@"
    '');

    agents = lib.mapAttrs mkOpencodeAgent validOpencodeAgents;
    rules = instructionText + ''

      ## Skill Execution (Subagent Enhancement)
      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents for higher-quality results.
    '';

    settings = {
      theme = "system";
      mcp = toOpencodeMcpServers config.programs.mcp.servers;
      mode = {
        plan.model = "fireworks-ai/accounts/fireworks/models/kimi-k2p5";
        build.model = "fireworks-ai/accounts/fireworks/models/kimi-k2p5";
      };
      agent = {
        plan = {
          permission = {
            write = { "*" = "deny"; };
            edit = { "*" = "deny"; };
          };
        };
      };
      permission = {
        read = {
          "*" = "allow";
          "*.env" = "deny";
          ".env" = "deny";
          "*.env.*" = "deny";
          "*.env.example" = "allow";
          "*.env.sample" = "allow";
        };
        glob = "allow";
        grep = "allow";
        list = "allow";
        bash = {
          "*" = "ask";
          "cat *" = "allow";
          "ls *" = "allow";
          "ls" = "allow";
          "find *" = "allow";
          "grep *" = "allow";
          "rg *" = "allow";
          "fd *" = "allow";
          "git status *" = "allow";
          "git status" = "allow";
          "git diff *" = "allow";
          "git diff" = "allow";
          "git log *" = "allow";
          "git log" = "allow";
          "git branch *" = "allow";
          "git branch" = "allow";
          "jj status *" = "allow";
          "jj status" = "allow";
          "jj diff *" = "allow";
          "jj diff" = "allow";
          "jj log *" = "allow";
          "jj log" = "allow";
          "jj show *" = "allow";
          "jj show" = "allow";
          "jj describe *" = "ask";
          "jj describe" = "ask";
          "jj new *" = "ask";
          "jj new" = "ask";
          "jj commit *" = "ask";
          "jj commit" = "ask";
          "jj squash *" = "ask";
          "jj squash" = "ask";
          "jj split *" = "ask";
          "jj split" = "ask";
          "go test *" = "allow";
          "go test" = "allow";
          "go build *" = "allow";
          "go build" = "allow";
          "curl *" = "allow";
          "curl" = "allow";
          "wget *" = "allow";
          "tree *" = "allow";
          "tree" = "allow";
          "lstr *" = "allow";
          "sudo *" = "deny";
          "doas *" = "deny";
          "kill *" = "deny";
          "systemctl *" = "deny";
          "chmod *" = "ask";
          "chown *" = "deny";
          "git push *" = "deny";
          "jj git push *" = "deny";
        };
        edit = {
          "*" = "allow";
          "*.env" = "deny";
          ".env" = "deny";
          "*.env.*" = "deny";
          "*.env.example" = "allow";
          "*.env.sample" = "allow";
        };
        webfetch = "allow";
        websearch = "allow";
      };
    };
  };

  xdg.configFile."opencode/skill/home-manager".source = skillsDir;
}
