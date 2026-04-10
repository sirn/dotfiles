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

  modelType = lib.types.submodule {
    options = {
      id = lib.mkOption {
        type = lib.types.str;
        description = "Unique identifier for the model.";
      };
      api = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "API type override for this model (defaults to provider's API type).";
      };
      baseUrl = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Base URL override for this model (defaults to provider's base URL).";
      };
      name = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable name for the model.";
      };
      family = lib.mkOption {
        type = lib.types.str;
        description = "Model family name (e.g., claude, gpt, gemini).";
      };
      reasoning = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether the model supports extended reasoning/thinking.";
      };
      input = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "text" ];
        description = "Supported input modalities (e.g., text, image, audio).";
      };
      contextWindow = lib.mkOption {
        type = lib.types.int;
        description = "Maximum context window size in tokens.";
      };
      maxTokens = lib.mkOption {
        type = lib.types.int;
        description = "Maximum output tokens for the model.";
      };
      costInput = lib.mkOption {
        type = lib.types.number;
        default = 0;
        description = "Cost per 1M input tokens in USD.";
      };
      costOutput = lib.mkOption {
        type = lib.types.number;
        default = 0;
        description = "Cost per 1M output tokens in USD.";
      };
      costCacheRead = lib.mkOption {
        type = lib.types.number;
        default = 0;
        description = "Cost per 1M cached input tokens read in USD.";
      };
      costCacheWrite = lib.mkOption {
        type = lib.types.number;
        default = 0;
        description = "Cost per 1M cached input tokens written in USD.";
      };
      attachment = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether the model supports file attachments.";
      };
      toolCall = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether the model supports tool/function calling.";
      };
      temperature = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether the model supports temperature parameter.";
      };
      reasoningEffort = lib.mkOption {
        type = lib.types.str;
        default = "medium";
        description = "Default reasoning effort level (low, medium, high).";
      };
      pi = lib.mkOption {
        type = lib.types.nullOr agentOverrideType;
        default = null;
        description = "Pi-specific model overrides.";
      };
      opencode = lib.mkOption {
        type = lib.types.nullOr agentOverrideType;
        default = null;
        description = "OpenCode-specific model overrides.";
      };
    };
  };

  providerType = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable name for the provider.";
      };
      baseUrl = lib.mkOption {
        type = lib.types.str;
        description = "Base URL for the provider API.";
      };
      envVar = lib.mkOption {
        type = lib.types.str;
        description = "Environment variable name for the API key.";
      };
      api = lib.mkOption {
        type = lib.types.str;
        description = "API type for the provider (e.g., anthropic-messages, openai-completions).";
      };
      reasoningEffort = lib.mkOption {
        type = lib.types.str;
        default = "medium";
        description = "Default reasoning effort level for the provider.";
      };
      compatibility = lib.mkOption {
        type = lib.types.submodule {
          options = {
            developerRole = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Whether the provider supports the developer role (vs system).";
            };
          };
        };
        default = { };
        description = "Provider compatibility flags consumed by tools that need them.";
      };
      models = lib.mkOption {
        type = lib.types.listOf modelType;
        default = [ ];
        description = "List of models available from this provider.";
      };
      pi = lib.mkOption {
        type = lib.types.nullOr agentOverrideType;
        default = null;
        description = "Pi-specific provider overrides.";
      };
      opencode = lib.mkOption {
        type = lib.types.nullOr agentOverrideType;
        default = null;
        description = "OpenCode-specific provider overrides.";
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

  opencodeAgentType = lib.types.submodule {
    options = {
      model = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Model ID to use for OpenCode subagent.";
      };
      primary = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether this is the primary subagent.";
      };
      mode = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Operating mode for the subagent.";
      };
      permission = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = null;
        description = "Permission configuration for the subagent.";
      };
    };
  };

  subagentType = lib.types.submodule {
    options = {
      description = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable description of the subagent's purpose.";
      };
      mode = lib.mkOption {
        type = lib.types.enum [
          "plan"
          "build"
        ];
        default = "plan";
        description = "Operating mode: plan for planning tasks, build for execution.";
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
      opencode = lib.mkOption {
        type = lib.types.nullOr opencodeAgentType;
        default = null;
        description = "OpenCode-specific configuration.";
      };
    };
  };
in
{
  options.agents = {
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

    subagents = lib.mkOption {
      type = lib.types.attrsOf subagentType;
      default = { };
      description = "Subagent definitions shared across agent tools.";
    };
  };
}
