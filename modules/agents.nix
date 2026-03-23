{ lib, config, ... }:

let
  cfg = config.agents;

  commandEntry =
    lib.types.coercedTo lib.types.str
      (match: {
        inherit match;
        mode = "prefix";
      })
      (
        lib.types.submodule {
          options = {
            match = lib.mkOption { type = lib.types.str; };
            mode = lib.mkOption {
              type = lib.types.enum [
                "exact"
                "prefix"
                "substring"
              ];
              default = "prefix";
            };
          };
        }
      );

  wrapperEntry = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      kind = lib.mkOption { type = lib.types.str; };
    };
  };

  commandsOptions = {
    allow = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
    };
    ask = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
    };
    deny = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
    };
  };

  pathListsOptions = {
    read = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
    edit = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
    write = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  modelType = lib.types.submodule {
    options = {
      id = lib.mkOption { type = lib.types.str; };
      name = lib.mkOption { type = lib.types.str; };
      family = lib.mkOption { type = lib.types.str; };
      reasoning = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      input = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "text" ];
      };
      contextWindow = lib.mkOption { type = lib.types.int; };
      maxTokens = lib.mkOption { type = lib.types.int; };
      costInput = lib.mkOption {
        type = lib.types.number;
        default = 0;
      };
      costOutput = lib.mkOption {
        type = lib.types.number;
        default = 0;
      };
      costCacheRead = lib.mkOption {
        type = lib.types.number;
        default = 0;
      };
      costCacheWrite = lib.mkOption {
        type = lib.types.number;
        default = 0;
      };
      attachment = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      toolCall = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      temperature = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      reasoningEffort = lib.mkOption {
        type = lib.types.str;
        default = "medium";
      };
    };
  };

  providerType = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      baseUrl = lib.mkOption { type = lib.types.str; };
      envVar = lib.mkOption { type = lib.types.str; };
      api = lib.mkOption { type = lib.types.str; };
      reasoningEffort = lib.mkOption {
        type = lib.types.str;
        default = "medium";
      };
      models = lib.mkOption {
        type = lib.types.listOf modelType;
        default = [ ];
      };
    };
  };

  modePermType = lib.types.submodule {
    options = {
      tools = lib.mkOption {
        type = lib.types.attrsOf lib.types.bool;
        default = { };
      };
      commands = lib.mkOption {
        type = lib.types.submodule { options = commandsOptions; };
        default = { };
      };
      wrappers = lib.mkOption {
        type = lib.types.listOf wrapperEntry;
        default = [ ];
      };
      redirects = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
      heredocs = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
    };
  };

  claudeCodeAgentType = lib.types.submodule {
    options = {
      allowedTools = lib.mkOption { type = lib.types.listOf lib.types.str; };
      color = lib.mkOption { type = lib.types.str; };
      model = lib.mkOption { type = lib.types.str; };
    };
  };

  opencodeAgentType = lib.types.submodule {
    options = {
      model = lib.mkOption {
        type = lib.types.str;
        default = "";
      };
      primary = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      mode = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
      permission = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = null;
      };
    };
  };

  subagentType = lib.types.submodule {
    options = {
      description = lib.mkOption { type = lib.types.str; };
      mode = lib.mkOption {
        type = lib.types.enum [
          "plan"
          "build"
        ];
        default = "plan";
      };
      prompt = lib.mkOption { type = lib.types.lines; };
      claude-code = lib.mkOption {
        type = lib.types.nullOr claudeCodeAgentType;
        default = null;
      };
      opencode = lib.mkOption {
        type = lib.types.nullOr opencodeAgentType;
        default = null;
      };
    };
  };

  d = cfg.permissions.default;

  mkEffective =
    _name: modeCfg:
    let
      merge = section: lib.unique (d.commands.${section} ++ modeCfg.commands.${section});
    in
    {
      tools = d.tools // modeCfg.tools;
      commands = {
        allow = merge "allow";
        ask = merge "ask";
        deny = merge "deny";
      };
      paths = d.paths;
      wrappers = d.wrappers ++ modeCfg.wrappers;
      redirects = if modeCfg.redirects != { } then modeCfg.redirects else d.redirects;
      heredocs = if modeCfg.heredocs != { } then modeCfg.heredocs else d.heredocs;
    };

in
{
  options.agents = {
    instructionText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Shared instruction text (AGENTS.md) for all agents.";
    };

    skillsDir = lib.mkOption {
      type = lib.types.path;
      description = "Path to the shared skills directory.";
    };

    domains.allowed = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Allowed domains for WebFetch (consumed by Claude Code).";
    };

    models = {
      default = {
        provider = lib.mkOption {
          type = lib.types.str;
          description = "Default provider name.";
        };
        model = lib.mkOption {
          type = lib.types.str;
          description = "Default model ID.";
        };
      };

      providers = lib.mkOption {
        type = lib.types.attrsOf providerType;
        default = { };
        description = "Provider configurations keyed by provider ID.";
      };
    };

    permissions = {
      default = lib.mkOption {
        type = lib.types.submodule {
          options = {
            tools = lib.mkOption {
              type = lib.types.attrsOf lib.types.bool;
              default = { };
            };
            commands = lib.mkOption {
              type = lib.types.submodule { options = commandsOptions; };
              default = { };
            };
            wrappers = lib.mkOption {
              type = lib.types.listOf wrapperEntry;
              default = [ ];
            };
            redirects = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            heredocs = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            paths = lib.mkOption {
              type = lib.types.submodule {
                options = {
                  allow = lib.mkOption {
                    type = lib.types.submodule { options = pathListsOptions; };
                    default = { };
                  };
                  deny = lib.mkOption {
                    type = lib.types.submodule { options = pathListsOptions; };
                    default = { };
                  };
                };
              };
              default = { };
            };
          };
        };
        default = { };
        description = "Default (base) permission policy.";
      };

      modes = lib.mkOption {
        type = lib.types.attrsOf modePermType;
        default = { };
        description = "Per-mode permission overrides merged on top of default.";
      };

      effective = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        readOnly = true;
        description = "Pre-computed effective policy per mode (default merged with mode overrides).";
      };
    };

    subagents = lib.mkOption {
      type = lib.types.attrsOf subagentType;
      default = { };
      description = "Subagent definitions shared across agent tools.";
    };
  };

  config.agents.permissions.effective = lib.mapAttrs mkEffective cfg.permissions.modes;
}
