{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;
  agentsCfg = config.agents;

  # Transform module model to Pi format
  toPiModel =
    m:
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
    // lib.optionalAttrs (m.api != null) { api = m.api; };

  # Build provider config from agents.models
  mkPiProvider =
    name: p:
    {
      baseUrl = p.baseUrl;
      apiKey = p.envVar;
      api = p.api;
      defaultThinkingLevel = p.reasoningEffort;
      models = map toPiModel p.models;
    }
    // lib.optionalAttrs (!p.compatibility.developerRole) { compat.supportsDeveloperRole = false; };

  piPackage = pkgs.unstable.pi-coding-agent;
  piVersion = piPackage.version or "0.0.0";
  isPi061orLater = lib.versionAtLeast piVersion "0.61.0";

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    export PI_SKIP_VERSION_CHECK=1
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe piPackage}" "$@"
  '';

  agentsMdText = ''
    ${agentsCfg.instructionText}
    ## Safety Guidelines (Pi-specific)

    - When running destructive commands (`rm`, etc.), you must first ask the user.
    - When doing a commit, ask user for confirmation first.
    - Do not squash commit unless being told explicitly by the user.
  '';

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

  # Combine bundled extensions with generated JSON config into a single derivation
  bundledAgent = pkgs.runCommand "pi-bundled-agent" { } ''
    mkdir -p $out/extensions/home-manager
    cp -r ${./extensions}/. $out/extensions/home-manager/
    cp ${policyJsonFile} $out/policy.json

    # Substitute keybinding names based on Pi version
    ${lib.optionalString (!isPi061orLater) ''
      substituteInPlace $out/extensions/home-manager/extensions/plan-mode.ts \
        --replace-fail '"__KEYBINDING_EXPAND_TOOLS__"' '"expandTools"'
    ''}
    ${lib.optionalString isPi061orLater ''
      substituteInPlace $out/extensions/home-manager/extensions/plan-mode.ts \
        --replace-fail '"__KEYBINDING_EXPAND_TOOLS__"' '"app.tools.expand"'
    ''}
  '';
in
{
  imports = [ ./keybindings.nix ];

  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    instructionText = agentsMdText;

    settings = {
      quietStartup = true;
      defaultProvider = agentsCfg.models.default.provider;
      defaultModel = agentsCfg.models.default.model;
      defaultThinkingLevel = "high";
      hideThinkingBlock = false;
      enabledModels = lib.concatMap (p: map (m: m.id) p.models) (
        builtins.attrValues agentsCfg.models.providers
      );
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    providers = lib.mapAttrs mkPiProvider agentsCfg.models.providers;
  };

  home.file = {
    ".pi/agent/skills/home-manager".source = agentsCfg.skillsDir;
    ".pi/agent/extensions/home-manager".source = "${bundledAgent}/extensions/home-manager";
    ".pi/agent/policy.json".source = "${bundledAgent}/policy.json";
  };

  # Pass the version check to the keybindings module
  _module.args = {
    inherit isPi061orLater;
  };
}
