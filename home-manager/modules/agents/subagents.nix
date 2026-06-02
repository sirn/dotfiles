{ lib, ... }:

let
  agentOverrideType = lib.types.submodule {
    options = {
      baseUrl = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Agent-specific base URL override.";
      };
    };
  };

  claudeCodeAgentType = lib.types.submodule {
    options = {
      allowedTools = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of allowed tools for Claude Code subagent.";
      };
      color = lib.mkOption {
        type = lib.types.str;
        description = "Color theme for the subagent.";
      };
      model = lib.mkOption {
        type = lib.types.str;
        description = "Model ID to use for this subagent.";
      };
    };
  };

  subagentType = lib.types.submodule {
    options = {
      description = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable description of the subagent's purpose.";
      };
      prompt = lib.mkOption {
        type = lib.types.lines;
        description = "System prompt for the subagent.";
      };

      claude-code = lib.mkOption {
        type = lib.types.nullOr claudeCodeAgentType;
        default = null;
        description = "Claude Code-specific configuration.";
      };
      pi = lib.mkOption {
        type = lib.types.submodule {
          options = {
            runner = lib.mkOption {
              type = lib.types.enum [
                "pi"
                "claude-code"
              ];
              default = "pi";
              description = "Which runner to use for executing this subagent. pi uses RPC mode, claude-code uses --print stream-json.";
            };
            tools = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Pi tools available to this subagent.";
            };
            model = lib.mkOption {
              type = lib.types.str;
              description = "Pi model ID for this subagent.";
            };
          };
        };
        description = "Pi-specific subagent configuration.";
      };
    };
  };
in
{
  options.agents.subagents = lib.mkOption {
    type = lib.types.attrsOf subagentType;
    default = { };
    description = "Subagent definitions shared across agent tools.";
  };
}
