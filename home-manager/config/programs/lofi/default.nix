{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;

  # lofi's dirs::config_dir() resolves to ~/Library/Application Support on
  # macOS, not ~/.config, so point it at the home-manager-managed config file.
  wrappedLofi = config.agents.sandbox.mkWrapper {
    name = "lofi";
    package = pkgs.local.lofi;
  };

  toCommandEntry =
    entry:
    if builtins.isString entry then
      {
        match = entry;
        mode = "prefix";
      }
    else
      {
        match = entry.match;
        mode = entry.mode;
      };

  toWrapperKind = kind: lib.replaceStrings [ "-" ] [ "_" ] kind;

  policyFromPermissions = {
    allow = map toCommandEntry agentsCfg.permissions.default.commands.allow;
    ask = map toCommandEntry agentsCfg.permissions.default.commands.ask;
    deny = map toCommandEntry agentsCfg.permissions.default.commands.deny;
    wrappers = map (w: {
      name = w.name;
      kind = toWrapperKind w.kind;
    }) agentsCfg.permissions.default.wrappers;
    redirects = {
      action = agentsCfg.permissions.default.redirects.action or "ask";
    };
    heredocs = {
      action = agentsCfg.permissions.default.heredocs.action or "ask";
    };
  };
in
{
  programs.lofi = {
    enable = true;

    package = wrappedLofi;

    instructionText = agentsCfg.instructionText;

    policy = policyFromPermissions;

    settings = {
      bash.pass_env = agentsCfg.requiredEnvs;
    };
  };

  home.file.".config/lofi/skills".source = config.agents.skillTrees;
}
