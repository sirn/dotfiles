{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;

  agentsDir = ../../../var/agents/agents;

  # Transform module model to OpenCode format
  toOpenCodeModel = m: {
    id = m.id;
    name = m.name;
    family = m.family;
    attachment = m.attachment;
    reasoning = m.reasoning;
    tool_call = m.toolCall;
    temperature = m.temperature;
    modalities = {
      input = m.input;
      output = [ "text" ];
    };
    limit = {
      context = m.contextWindow;
      output = m.maxTokens;
    };
    options = {
      reasoningEffort = m.reasoningEffort;
    };
  };

  # Build OpenCode provider config from agents.models (all providers use openai-compatible)
  mkOpenCodeProvider = name: p: {
    npm = "@ai-sdk/openai-compatible";
    name = p.name;
    options = {
      baseURL = "${p.baseUrl}/v1";
      apiKey = "{env:${p.envVar}}";
    };
    models = builtins.listToAttrs (map (m: lib.nameValuePair m.id (toOpenCodeModel m)) p.models);
  };

  policy = agentsCfg.permissions.effective.build;

  toOpencodePermissions =
    let
      inherit (policy) tools commands paths;

      mkPathRules =
        section:
        lib.listToAttrs (map (p: lib.nameValuePair p "allow") (paths.allow.${section} or [ ]))
        // lib.listToAttrs (map (p: lib.nameValuePair p "deny") (paths.deny.${section} or [ ]))
        // {
          "*" = "allow";
        };

      mkBashRules =
        let
          mkEntries =
            decision: cmds:
            lib.concatMap (
              entry:
              let
                m = entry.match;
              in
              {
                exact = [ (lib.nameValuePair m decision) ];
                prefix = [
                  (lib.nameValuePair "${m} *" decision)
                  (lib.nameValuePair m decision)
                ];
                substring = [
                  (lib.nameValuePair "* ${m} *" decision)
                  (lib.nameValuePair "${m} *" decision)
                  (lib.nameValuePair "* ${m}" decision)
                ];
              }
              .${entry.mode or "prefix"}
            ) cmds;
          allows = mkEntries "allow" (commands.allow or [ ]);
          asks = mkEntries "ask" (commands.ask or [ ]);
          denies = mkEntries "deny" (commands.deny or [ ]);
        in
        lib.listToAttrs (allows ++ asks ++ denies) // { "*" = "ask"; };
    in
    {
      read = mkPathRules "read";
      glob = "allow";
      grep = "allow";
      list = "allow";
      bash = mkBashRules;
      edit = if tools.edit then (mkPathRules "edit") else (mkPathRules "edit" // { "*" = "deny"; });
      webfetch = "allow";
      websearch = "allow";
    };

  mkOpencodeAgent =
    name: agent:
    let
      oc = agent.opencode;
      isPrimary = (oc.primary or false);
      modelLine = lib.optionalString (isPrimary && oc.model != "") "model: ${oc.model}\n";
      modeVal = if !isPrimary then "subagent" else (oc.mode or "primary");
      modeLine = lib.optionalString (modeVal != "") "mode: ${modeVal}\n";
      permissionLine = lib.optionalString (
        oc.permission != null
      ) "permission: ${builtins.toJSON oc.permission}\n";
    in
    ''
      ---
      description: ${agent.description}
      ${modelLine}${modeLine}${permissionLine}---
      ${agent.prompt}
    '';

  opencodeAgents = lib.filterAttrs (name: agent: agent.opencode != null) agentsCfg.subagents;

  validateOpencodeAgent =
    name: agent: if !(agent ? description) then throw "Agent ${name}: missing 'description'" else agent;

  validOpencodeAgents = lib.mapAttrs validateOpencodeAgent opencodeAgents;

  isStdioServer = server: server ? command || server ? package;

  toOpencodeMcpServers =
    servers:
    lib.mapAttrs (
      name: server:
      if isStdioServer server then
        {
          command = [ (server.command or (lib.getExe server.package)) ];
          type = "local";
          enabled = true;
        }
      else
        {
          url = server.url;
          type = "remote";
          enabled = true;
        }
    ) servers;

  # Generate MCP tool permissions from server allowedTools
  opencodeMcpPermissions = lib.listToAttrs (
    lib.flatten (
      lib.mapAttrsToList (
        name: server:
        let
          tools = server.allowedTools or null;
        in
        if tools == null then
          [
            {
              name = "${name}_*";
              value = true;
            }
          ]
        else
          # Deny all tools by default, allow specific ones
          [
            {
              name = "${name}_*";
              value = false;
            }
          ]
          ++ (map (tool: {
            name = "${name}_${tool}";
            value = true;
          }) tools)
      ) config.programs.mcp.servers
    )
  );
in
{
  programs.opencode = {
    enable = true;

    package = (
      pkgs.writeScriptBin "opencode" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
          -- "${lib.getExe pkgs.local.opencode-bin}" "$@"
      ''
    );

    agents = lib.mapAttrs mkOpencodeAgent validOpencodeAgents;
    rules = agentsCfg.instructionText + ''

      ## Skill Execution (Subagent Enhancement)

      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents for higher-quality results.
    '';

    settings = {
      theme = "system";
      mcp = toOpencodeMcpServers config.programs.mcp.servers;
      mode = {
        plan.model = "${agentsCfg.models.default.provider}/${agentsCfg.models.default.model}";
        build.model = "${agentsCfg.models.default.provider}/${agentsCfg.models.default.model}";
      };
      permission = toOpencodePermissions;
      tools = opencodeMcpPermissions;
      provider = lib.mapAttrs mkOpenCodeProvider agentsCfg.models.providers;
    };
  };

  xdg.configFile."opencode/skill/home-manager".source = agentsCfg.skillsDir;
}
