{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;
  agentsCfg = config.agents;

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

  # Plan mode templates
  planModeTemplates = {
    prompt = builtins.readFile ./PLAN_PROMPT.md;
    accept = builtins.readFile ./PLAN_ACCEPT.md;
    subsequent = builtins.readFile ./PLAN_INJECT.md;
  };

  policyAutoModePrompt = builtins.readFile ./POLICY_AUTO_MODE.md;
  policyAutoModeExtraCommands = lib.strings.trim agentsCfg.commandContext;
  policyAutoModeContextFiles = lib.filterAttrs (
    name: type:
    type == "regular" && lib.hasPrefix "POLICY_AUTO_MODE." name && lib.hasSuffix "_CONTEXT.md" name
  ) (builtins.readDir ./.);
  policyAutoModeContextFileEntries = lib.mapAttrs' (
    name: _:
    lib.nameValuePair ".pi/agent/custom/execution-policy/${name}" {
      text = builtins.readFile (./. + "/${name}");
    }
  ) policyAutoModeContextFiles;

  # Generate pi-compatible agent markdown files from subagent configs
  piAgentMdFiles = builtins.listToAttrs (
    builtins.filter (a: a != null) (
      builtins.attrValues (
        builtins.mapAttrs (
          name: agentCfg:
          if agentCfg ? pi then
            let
              piCfg = agentCfg.pi;
              tools = builtins.concatStringsSep ", " piCfg.tools;
              content = ''
                ---
                name: ${name}
                description: ${agentCfg.description}
                tools: ${tools}
                model: ${piCfg.model}
                ---
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

    settings = {
      quietStartup = true;
      hideThinkingBlock = false;
      theme = config.home.colors.variant;
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    defaultProvider = lib.mkDefault "google";
    defaultModel = lib.mkDefault "gemini-3-flash-preview";
    defaultThinkingLevel = lib.mkDefault "high";

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
      ".pi/agent/skills".source = agentsCfg.skillTrees.subagent;
      ".pi/agent/custom/execution-policy/policy.json".source = policyJsonFile;
      ".pi/agent/custom/execution-policy/PLAN_PROMPT.md".text = planModeTemplates.prompt;
      ".pi/agent/custom/execution-policy/PLAN_ACCEPT.md".text = planModeTemplates.accept;
      ".pi/agent/custom/execution-policy/PLAN_INJECT.md".text = planModeTemplates.subsequent;
      ".pi/agent/custom/execution-policy/POLICY_AUTO_MODE.md".text = policyAutoModePrompt;
      ".pi/agent/custom/execution-policy/POLICY_AUTO_MODE.COMMANDS_CONTEXT.md".text = lib.mkIf (
        policyAutoModeExtraCommands != ""
      ) policyAutoModeExtraCommands;
    }
    // policyAutoModeContextFileEntries
    // piAgentMdFiles;
}
