{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;

  agentsCfg = config.agents;

  # YAML-safe: quote strings containing colons, hashes, or other special chars
  yamlQuote = s: if builtins.match "^[a-zA-Z0-9_/., -]+$" s != null then s else "\"${s}\"";

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    export PI_SKIP_VERSION_CHECK=1
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
      -- "${lib.getExe pkgs.local.pi-coding-agent}" "$@"
  '';

  agentsMdText = agentsCfg.instructionText;

  # Resolve baseUrl override for Pi: model.pi > model > provider.pi
  # Returns null if no override is set (falls back to provider default)
  resolvePiBaseUrl =
    p: m:
    if m.pi != null && m.pi.baseUrl != null then
      m.pi.baseUrl
    else if m.baseUrl != null then
      m.baseUrl
    else if p.pi != null && p.pi.baseUrl != null then
      p.pi.baseUrl
    else
      null;

  # Build model-level compat attrset (only when explicitly set)
  mkModelCompat =
    m:
    let
      c = m.compatibility;
    in
    lib.optionalAttrs (c != null) (
      lib.optionalAttrs (c.developerRole != null) { supportsDeveloperRole = c.developerRole; }
    );

  # Transform module model to Pi format
  toPiModel =
    p: m:
    {
      id = m.id;
      name = m.name;
      reasoning = m.reasoning;
      input = m.input;
      contextWindow = m.contextWindow;
      maxTokens = m.maxTokens;
      cost = {
        input = m.costInput;
        output = m.costOutput;
        cacheRead = m.costCacheRead;
        cacheWrite = m.costCacheWrite;
      };
    }
    // lib.optionalAttrs (m.api != null) { api = m.api; }
    // lib.optionalAttrs (resolvePiBaseUrl p m != null) { baseUrl = resolvePiBaseUrl p m; }
    // lib.optionalAttrs (mkModelCompat m != { }) { compat = mkModelCompat m; };

  # Build provider config from agents.models
  mkPiProvider =
    name: p:
    lib.optionalAttrs (p.baseUrl != null) { baseUrl = p.baseUrl; }
    // lib.optionalAttrs (p.api != null) { api = p.api; }
    // {
      models = map (toPiModel p) p.models;
    }
    // lib.optionalAttrs (p.envVar != null) { apiKey = p.envVar; }
    // lib.optionalAttrs (p.headers != { }) { headers = p.headers; }
    // lib.optionalAttrs (!p.compatibility.developerRole) { compat.supportsDeveloperRole = false; };

  perms = agentsCfg.permissions;

  # Generate unified policy JSON for all extensions
  policyJson = builtins.toJSON {
    default = {
      commands = {
        allow = perms.default.commands.allow;
        ask = perms.default.commands.ask;
        deny = perms.default.commands.deny;
      };
      wrappers = perms.default.wrappers;
      redirects = perms.default.redirects;
      heredocs = perms.default.heredocs;
    };
    modes = lib.mapAttrs (_name: modePerms: {
      tools = modePerms.tools;
      commands = {
        deny = modePerms.commands.deny;
        ask = modePerms.commands.ask;
        allow = modePerms.commands.allow;
      };
      wrappers = perms.default.wrappers ++ modePerms.wrappers;
      redirects = if modePerms.redirects != { } then modePerms.redirects else perms.default.redirects;
      heredocs = if modePerms.heredocs != { } then modePerms.heredocs else perms.default.heredocs;
    }) perms.modes;
  };

  # Write JSON file to store path (safer than echo in shell)
  policyJsonFile = pkgs.writeTextFile {
    name = "policy.json";
    text = policyJson;
  };

  policyAutoModePrompt = builtins.readFile ./auto-mode/prompt.md;
  policyAutoModeExtraCommands = lib.strings.trim agentsCfg.commandContext;
  policyAutoModeContextFiles = builtins.listToAttrs (
    builtins.map
      (
        name: lib.nameValuePair "auto-mode/${name}" { text = builtins.readFile (./auto-mode + "/${name}"); }
      )
      (
        builtins.filter (
          n: builtins.match ".*\\.md$" n != null && n != "prompt.md" && n != "commands-context.md"
        ) (builtins.attrNames (builtins.readDir ./auto-mode))
      )
  );
  policyAutoModeContextFileEntries = lib.mapAttrs' (
    name: value: lib.nameValuePair ".pi/agent/custom/execution-policy/${name}" value
  ) policyAutoModeContextFiles;

  # Generate agent markdown files from subagent configs
  # pi.runner determines the frontmatter format:
  #   "pi"          -> standard Pi .md
  #   "claude-code" -> adds runner field; tools are CC-style names
  piAgentMdFiles = builtins.listToAttrs (
    builtins.filter (a: a != null) (
      builtins.attrValues (
        builtins.mapAttrs (
          name: agentCfg:
          if agentCfg.pi.runner == "pi" then
            let
              piCfg = agentCfg.pi;
              tools = builtins.concatStringsSep ", " piCfg.tools;
              content = ''
                ---
                name: ${name}
                description: ${yamlQuote agentCfg.description}
                tools: ${tools}
                model: ${piCfg.model}
                ---
                ${agentsCfg.subagentPreamble}

                ${agentCfg.prompt}'';
            in
            {
              name = ".pi/agent/agents/${name}.md";
              value = {
                text = content;
              };
            }
          else if agentCfg.pi.runner == "claude-code" then
            let
              piCfg = agentCfg.pi;
              tools = builtins.concatStringsSep ", " piCfg.tools;
              content = ''
                ---
                name: ${name}
                description: ${yamlQuote agentCfg.description}
                runner: claude-code
                tools: ${tools}
                model: ${piCfg.model}
                ---
                ${agentsCfg.subagentPreamble}

                ${agentCfg.prompt}'';
            in
            {
              name = ".pi/agent/agents/${name}.md";
              value = {
                text = content;
              };
            }
          else
            null
        ) agentsCfg.subagents
      )
    )
  );
in

{
  imports = [ ./keybindings.nix ];

  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    instructionText = agentsMdText;

    defaultProvider = lib.mkIf (agentsCfg.models.default != null) agentsCfg.models.default.provider;

    defaultModel = lib.mkIf (agentsCfg.models.default != null) agentsCfg.models.default.model;

    defaultThinkingLevel = lib.mkIf (
      agentsCfg.models.default != null
      && builtins.hasAttr agentsCfg.models.default.provider agentsCfg.models.providers
    ) agentsCfg.models.providers.${agentsCfg.models.default.provider}.reasoningEffort;

    settings = {
      quietStartup = true;
      hideThinkingBlock = true;
      theme = config.home.colors.variant;
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    providers = lib.mapAttrs mkPiProvider agentsCfg.models.providers;

    extensionCustom = {
      "subagent".maxAgentsPerStep = 3;
    };
  };

  home.file =
    lib.mapAttrs' (
      name: _:
      lib.nameValuePair ".pi/agent/extensions/hm-${name}" {
        source = ./extensions/home-manager + "/${name}";
      }
      # Note: builtins.readDir ordering is non-deterministic per Nix spec,
      # but home.file deployment order does not matter here.
    ) (builtins.readDir ./extensions/home-manager)
    // {
      ".pi/agent/extensions/rimuruw-pi-hashline-edit".source = pkgs.local.pi-hashline-edit;
      ".pi/agent/skills".source = agentsCfg.skillTrees;
      ".pi/agent/custom/execution-policy/policy.json".source = policyJsonFile;

      ".pi/agent/custom/execution-policy/auto-mode/prompt.md".text = policyAutoModePrompt;
      ".pi/agent/custom/execution-policy/auto-mode/commands-context.md".text = lib.mkIf (
        policyAutoModeExtraCommands != ""
      ) policyAutoModeExtraCommands;
    }
    // policyAutoModeContextFileEntries
    // piAgentMdFiles;
}
