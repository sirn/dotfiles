{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;
  perms = agentsCfg.permissions;

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

  policyJsonFile = pkgs.writeTextFile {
    name = "policy.json";
    text = policyJson;
  };

  # Auto-mode prompt and context files
  autoModeDir = ../vendor/prompts/auto-mode;

  # YOLO / shell-policy prompt files
  shellPolicyPromptsDir = ../vendor/prompts/shell-policy;
  shellPolicyPromptEntries = builtins.listToAttrs (
    builtins.map
      (
        name:
        lib.nameValuePair ".pi/agent/custom/shell-policy/prompts/${name}" {
          text = builtins.readFile (shellPolicyPromptsDir + "/${name}");
        }
      )
      (
        builtins.filter (n: builtins.match ".*\\.md$" n != null) (
          builtins.attrNames (builtins.readDir shellPolicyPromptsDir)
        )
      )
  );

  policyAutoModePrompt = builtins.readFile (autoModeDir + "/prompt.md");
  policyAutoModeExtraCommands = lib.strings.trim agentsCfg.commandContext;
  policyAutoModeContextFiles = builtins.listToAttrs (
    builtins.map
      (
        name: lib.nameValuePair "auto-mode/${name}" { text = builtins.readFile (autoModeDir + "/${name}"); }
      )
      (
        builtins.filter (n: builtins.match ".*\\.md$" n != null && n != "prompt.md" && n != "commands.md") (
          builtins.attrNames (builtins.readDir autoModeDir)
        )
      )
  );
  policyAutoModeContextFileEntries = lib.mapAttrs' (
    name: value: lib.nameValuePair ".pi/agent/custom/shell-policy/${name}" value
  ) policyAutoModeContextFiles;
in

{
  home.file = {
    ".pi/agent/custom/shell-policy/policy.json".source = policyJsonFile;
    ".pi/agent/custom/shell-policy/auto-mode/prompt.md".text = policyAutoModePrompt;
    ".pi/agent/custom/shell-policy/auto-mode/commands.md".text = lib.mkIf (
      policyAutoModeExtraCommands != ""
    ) policyAutoModeExtraCommands;
  }
  // policyAutoModeContextFileEntries
  // {
    ".pi/agent/extensions/hm-shell-policy".source = ../vendor/extensions/shell-policy;
  }
  // shellPolicyPromptEntries;
}
