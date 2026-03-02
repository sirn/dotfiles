{ config, lib, pkgs, ... }:

let
  cfg = config.programs.pi-coding-agent;

  skillsDir = ../../../var/agents/skills;
  instructionText = builtins.readFile ../../../var/agents/instruction.md;
  permissionsToml = lib.importTOML ../../../var/agents/permissions.toml;

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe cfg.package}" "$@"
  '';

  # Base extensions that are always loaded
  baseExtensions = [
    "extensions/safety-gate.ts"
  ];

  # Combine base extensions with user-configured extensions
  allExtensions = baseExtensions ++ cfg.extensions;

  settingsJson = builtins.toJSON {
    quietStartup = true;
    defaultProvider = "synthetic";
    defaultModel = "hf:moonshotai/Kimi-K2.5";
    defaultThinkingLevel = "medium";
    hideThinkingBlock = true;
    enabledModels = [
      "accounts/fireworks/models/*"
      "hf:zai-org/*"
      "hf:moonshotai/*"
      "hf:MiniMaxAI/*"
      "claude-opus-4-6"
      "claude-sonnet-4-6"
      "gpt-5.3-codex"
      "gpt-5.3-codex-spark"
      "gemini-3.1-pro-preview"
      "gemini-3-flash-preview"
    ];
    skills = [
      "skills/home-manager"
    ]
    ++ (map (name: "skills/mcp/mcp-${name}") (builtins.attrNames config.programs.mcp.servers));
    extensions = allExtensions;
  };

  modelsJson = builtins.toJSON {
    providers = {
      synthetic = {
        baseUrl = "https://api.synthetic.new/openai/v1";
        apiKey = "SYNTHETIC_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "hf:moonshotai/Kimi-K2.5";
            name = "Kimi K2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:zai-org/GLM-4.7";
            name = "GLM 4.7 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:MiniMaxAI/MiniMax-M2.5";
            name = "MiniMax M2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1000000;
            maxTokens = 32768;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
      fireworks = {
        baseUrl = "https://api.fireworks.ai/inference/v1";
        apiKey = "FIREWORKS_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "accounts/fireworks/models/kimi-k2p5";
            name = "Kimi K2.5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0.6;
              output = 3.0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "accounts/fireworks/models/glm-5";
            name = "GLM 5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 1.0;
              output = 3.2;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
    };
  };

  agentsMdText = ''
    ${instructionText}

    ## MCP Skills (Pi-specific)
    - MCP skills are located in `~/.pi/agent/skills/mcp/mcp-<name>/` directories.
    - Each MCP skill provides a `./mcp-wrapper.sh` script for tool discovery and execution.
    - **ALWAYS run `./mcp-wrapper.sh list` first to discover available tools before attempting to call any tool.**
    - After discovering tools, execute with: `./mcp-wrapper.sh call <tool-name> '<json-arguments>'`

    ## Safety Guidelines (Pi-specific)
    - When running destructive commands (`rm`, etc.), you must first ask the user.
    - When doing a commit, ask user for confirmation first.
    - Do not squash commit unless being told explicitly by the user.
  '';

  mcpWrapperTemplate = builtins.readFile ./mcp-wrapper.sh;
  mcpExecStdioTemplate = builtins.readFile ./mcp-exec-stdio.sh;
  mcpExecSseTemplate = builtins.readFile ./mcp-exec-sse.sh;

  isStdioServer = server: server ? command || server ? package;

  toPiMcpWrapperScript = name: server:
    let
      isStdio = isStdioServer server;
      serverCmd = if isStdio then (server.command or (lib.getExe server.package)) else "";
      serverUrl = if isStdio then "" else server.url;
      jqBin = lib.getExe pkgs.jq;
      mcpRemoteBin = lib.getExe pkgs.local.mcpServers.mcp-remote;
      timeoutBin = lib.getExe' pkgs.coreutils "timeout";

      transportExec =
        if isStdio then
          builtins.replaceStrings
            [ "__SERVER_CMD__" "__JQ_BIN__" ]
            [ serverCmd jqBin ]
            mcpExecStdioTemplate
        else
          builtins.replaceStrings
            [ "__TIMEOUT_BIN__" "__MCP_REMOTE_BIN__" "__SERVER_URL__" "__JQ_BIN__" ]
            [ timeoutBin mcpRemoteBin serverUrl jqBin ]
            mcpExecSseTemplate;
    in
    pkgs.writeShellScript "mcp-${name}-wrapper" (
      builtins.replaceStrings
        [ "__MCP_NAME__" "__MCP_TRANSPORT__" "__JQ_BIN__" "__MCP_REMOTE_BIN__" "__TIMEOUT_BIN__" "__SERVER_CMD__" "__SERVER_URL__" "__TRANSPORT_EXEC__" ]
        [ name (if isStdio then "stdio" else "sse") jqBin mcpRemoteBin timeoutBin serverCmd serverUrl transportExec ]
        mcpWrapperTemplate
    );

  toPiMcpSkillText = name: ''
    ---
    name: mcp-${name}
    description: Skill to call ${name} MCP server. Run ./mcp-wrapper.sh list to discover available tools, then ./mcp-wrapper.sh call <tool-name> '<json-arguments>'.
    ---

    ## Tool discovery
    ./mcp-wrapper.sh list

    ## Tool execution
    ./mcp-wrapper.sh call <tool-name> '<json-arguments>'

    ## Example
    ./mcp-wrapper.sh call search '{"query": "example"}'
  '';

  mkMcpSkillFiles = name: server: {
    ".pi/agent/skills/mcp/mcp-${name}/SKILL.md".text = toPiMcpSkillText name;
    ".pi/agent/skills/mcp/mcp-${name}/mcp-wrapper.sh".source = toPiMcpWrapperScript name server;
  };

  mcpSkillFiles = lib.mkMerge (lib.mapAttrsToList mkMcpSkillFiles config.programs.mcp.servers);

  # Generate MCP allow commands from config.programs.mcp.servers
  # allowedTools = null means allow all, list means only specific tools
  # Note: "list" is always allowed (read-only discovery operation)
  mcpAllowCommands = lib.flatten (lib.mapAttrsToList
    (name: server:
      let tools = server.allowedTools or null; in
      [ "mcp-${name}/mcp-wrapper.sh list" ] ++
      (if tools == null
      then [ "mcp-${name}/mcp-wrapper.sh call" ]
      else map (tool: "mcp-${name}/mcp-wrapper.sh call ${tool}") tools)
    )
    config.programs.mcp.servers);

  # Generate JSON config for safety-gate extension
  safetyGateJson = builtins.toJSON {
    allow = permissionsToml.default.commands.allow.shell ++ mcpAllowCommands;
    ask = permissionsToml.default.commands.ask.shell;
    deny = permissionsToml.default.commands.deny.shell;
  };

  # Load static TypeScript extension
  safetyGateTs = builtins.readFile ./safety-gate.ts;

  baseFiles = {
    ".pi/agent/settings.json".text = settingsJson;
    ".pi/agent/models.json".text = modelsJson;
    ".pi/agent/AGENTS.md".text = cfg.instructionText;
    ".pi/agent/skills/home-manager".source = skillsDir;
    ".pi/agent/extensions/safety-gate.ts".text = safetyGateTs;
    ".pi/agent/extensions/safety-gate.json".text = safetyGateJson;
  };
in
{
  programs.pi-coding-agent = {
    enable = true;
    instructionText = agentsMdText;
  };

  home.packages = [ wrappedPi ];

  programs.git.ignores = [ ".pi/" ];

  home.file = lib.mkMerge [
    baseFiles
    mcpSkillFiles
  ];
}
