{ config, lib, ... }:

let
  readMdFiles = import ./lib/read-md-files.nix { inherit lib; };
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

  # Auto-mode prompt and context files
  autoModeDir = ../vendor/prompts/auto-mode;

  # YOLO / shell-policy prompt files
  shellPolicyPromptsDir = ../vendor/prompts/shell-policy;
in

{
  home.file = {
    ".pi/agent/custom/shell-policy/policy.json".text = policyJson;
    ".pi/agent/custom/shell-policy/auto-mode/prompt.md".text = builtins.readFile (
      autoModeDir + "/prompt.md"
    );
    ".pi/agent/custom/shell-policy/auto-mode/commands.md".text = lib.mkIf (
      lib.strings.trim agentsCfg.commandContext != ""
    ) (lib.strings.trim agentsCfg.commandContext);
  }
  // readMdFiles {
    dir = shellPolicyPromptsDir;
    prefix = ".pi/agent/custom/shell-policy/prompts";
  }
  // readMdFiles {
    dir = autoModeDir;
    prefix = ".pi/agent/custom/shell-policy/auto-mode";
    exclude = [
      "prompt.md"
      "commands.md"
    ];
  }
  // {
    ".pi/agent/extensions/hm-shell-policy".source = ../vendor/extensions/shell-policy;
  };
}
