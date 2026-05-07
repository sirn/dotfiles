{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;
  agentsCfg = config.agents;

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
    {
      baseUrl = p.baseUrl;
      api = p.api;
      models = map (toPiModel p) p.models;
    }
    // lib.optionalAttrs (p.envVar != null) { apiKey = p.envVar; }
    // lib.optionalAttrs (!p.compatibility.developerRole) { compat.supportsDeveloperRole = false; };

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    export PI_SKIP_VERSION_CHECK=1
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
      -- "${lib.getExe pkgs.unstable.pi-coding-agent}" "$@"
  '';

  agentsMdText = agentsCfg.instructionText;

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
    modes.plan = {
      tools = perms.modes.plan.tools;
      commands = {
        deny = perms.modes.plan.commands.deny;
        ask = perms.modes.plan.commands.ask;
        allow = perms.modes.plan.commands.allow;
      };
      wrappers = perms.default.wrappers ++ perms.modes.plan.wrappers;
      redirects = perms.modes.plan.redirects;
      heredocs = perms.modes.plan.heredocs;
    };
  };

  # Write JSON file to store path (safer than echo in shell)
  policyJsonFile = pkgs.writeTextFile {
    name = "policy.json";
    text = policyJson;
  };

  # Plan mode templates
  planModeTemplates = {
    prompt = builtins.readFile ./PLAN_PROMPT.md;
    accept = builtins.readFile ./PLAN_ACCEPT.md;
    subsequent = builtins.readFile ./PLAN_INJECT.md;
  };

  policyAutoModePrompt = builtins.readFile ./POLICY_AUTO_MODE.md;
  policyAutoModePlanContext = builtins.readFile ./POLICY_AUTO_MODE.PLAN_CONTEXT.md;
  policyAutoModeExtraCommands = lib.strings.trim agentsCfg.commandContext;
in

{
  imports = [ ./keybindings.nix ];

  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    instructionText = agentsMdText;

    settings = lib.mkMerge [
      {
        quietStartup = true;
        hideThinkingBlock = false;
        theme = config.home.colors.variant;
        enabledModels = lib.concatMap (p: map (m: m.id) p.models) (
          builtins.attrValues agentsCfg.models.providers
        );
        retry = {
          maxRetries = 10;
          maxDelayMs = 0;
        };
      }
      (lib.mkIf (agentsCfg.models.default != null) {
        defaultProvider = agentsCfg.models.default.provider;
        defaultModel = agentsCfg.models.default.model;
      })
      (lib.mkIf
        (
          agentsCfg.models.default != null
          && builtins.hasAttr agentsCfg.models.default.provider agentsCfg.models.providers
        )
        {
          defaultThinkingLevel =
            agentsCfg.models.providers.${agentsCfg.models.default.provider}.reasoningEffort;
        }
      )
    ];

    providers = lib.mapAttrs mkPiProvider agentsCfg.models.providers;
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
      ".pi/agent/skills".source = agentsCfg.skillTrees.default;
      ".pi/agent/custom/execution-policy/policy.json".source = policyJsonFile;
      ".pi/agent/custom/execution-policy/PLAN_PROMPT.md".text = planModeTemplates.prompt;
      ".pi/agent/custom/execution-policy/PLAN_ACCEPT.md".text = planModeTemplates.accept;
      ".pi/agent/custom/execution-policy/PLAN_INJECT.md".text = planModeTemplates.subsequent;
      ".pi/agent/custom/execution-policy/POLICY_AUTO_MODE.md".text = policyAutoModePrompt;
      ".pi/agent/custom/execution-policy/POLICY_AUTO_MODE.PLAN_CONTEXT.md".text =
        policyAutoModePlanContext;
      ".pi/agent/custom/execution-policy/POLICY_AUTO_MODE.COMMANDS_CONTEXT.md".text = lib.mkIf (
        policyAutoModeExtraCommands != ""
      ) policyAutoModeExtraCommands;
    };
}
