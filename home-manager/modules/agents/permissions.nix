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
            match = lib.mkOption {
              type = lib.types.str;
              description = "Pattern to match against command.";
            };
            mode = lib.mkOption {
              type = lib.types.enum [
                "exact"
                "prefix"
                "substring"
                "args"
              ];
              default = "prefix";
              description = "Match mode: exact, prefix, substring, or args.";
            };
          };
        }
      );

  wrapperEntry = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "Name of the wrapper command.";
      };
      kind = lib.mkOption {
        type = lib.types.str;
        description = "Kind/type of the wrapper.";
      };
    };
  };

  commandsOptions = {
    allow = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
      description = "Commands allowed without prompting.";
    };
    ask = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
      description = "Commands that require user confirmation.";
    };
    deny = lib.mkOption {
      type = lib.types.listOf commandEntry;
      default = [ ];
      description = "Commands that are always denied.";
    };
  };

  pathListsOptions = {
    read = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Paths allowed for read operations.";
    };
    edit = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Paths allowed for edit operations.";
    };
    write = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Paths allowed for write operations.";
    };
  };

  modePermType = lib.types.submodule {
    options = {
      tools = lib.mkOption {
        type = lib.types.attrsOf lib.types.bool;
        default = { };
        description = "Tools enabled/disabled for this mode.";
      };
      commands = lib.mkOption {
        type = lib.types.submodule { options = commandsOptions; };
        default = { };
        description = "Command permissions for this mode.";
      };
      wrappers = lib.mkOption {
        type = lib.types.listOf wrapperEntry;
        default = [ ];
        description = "Wrapper configurations for this mode.";
      };
      redirects = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = "Command redirects for this mode.";
      };
      heredocs = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = "Heredoc configurations for this mode.";
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
  options.agents.permissions = {
    default = lib.mkOption {
      type = lib.types.submodule {
        options = {
          tools = lib.mkOption {
            type = lib.types.attrsOf lib.types.bool;
            default = { };
            description = "Tools enabled/disabled by default.";
          };
          commands = lib.mkOption {
            type = lib.types.submodule { options = commandsOptions; };
            default = { };
            description = "Default command permissions.";
          };
          wrappers = lib.mkOption {
            type = lib.types.listOf wrapperEntry;
            default = [ ];
            description = "Default wrapper configurations.";
          };
          redirects = lib.mkOption {
            type = lib.types.attrs;
            default = { };
            description = "Default command redirects.";
          };
          heredocs = lib.mkOption {
            type = lib.types.attrs;
            default = { };
            description = "Default heredoc configurations.";
          };
          paths = lib.mkOption {
            type = lib.types.submodule {
              options = {
                allow = lib.mkOption {
                  type = lib.types.submodule { options = pathListsOptions; };
                  default = { };
                  description = "Paths allowed for operations.";
                };
                deny = lib.mkOption {
                  type = lib.types.submodule { options = pathListsOptions; };
                  default = { };
                  description = "Paths denied for operations.";
                };
              };
            };
            default = { };
            description = "Path-based permission rules.";
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

  config.agents.permissions.effective = lib.mapAttrs mkEffective cfg.permissions.modes;
}
