{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.programs.lofi;
  tomlFormat = pkgs.formats.toml { };

  permissionType = lib.types.submodule {
    options = {
      path = lib.mkOption {
        type = lib.types.str;
        example = "~/notes";
        description = "Directory tree granted to the file tools outside the workspace root. ~ expands to the home directory.";
      };
      access = lib.mkOption {
        type = lib.types.enum [
          "read"
          "write"
        ];
        default = "read";
        description = "Access level the grant allows. Defaults to read so an omitted field never widens to write.";
      };
    };
  };

  autoModeType = lib.types.nullOr (
    lib.types.submodule {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          description = "Whether to enable LLM-based auto mode pre-approval.";
        };
        provider = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          example = "openai";
          description = "LLM provider used for auto mode approval. Omit it when model is a qualified model query.";
        };
        model = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          example = "gpt-5.6-luna";
          description = "Chat model used for auto mode approval. Accepts a model id or a provider/model query. Required unless decision_model is set.";
        };
        decision_model = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          example = "plexus/clef-flash";
          description = "Decision model that gates commands with typed questions before the chat evaluator runs. Accepts a model id or a model query. Required unless model is set.";
        };
        max_tokens = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = null;
          example = 256;
          description = "Maximum number of tokens for an auto mode response. Defaults to the selected model's output limit.";
        };
        timeout_ms = lib.mkOption {
          type = lib.types.int;
          default = 30000;
          example = 30000;
          description = "Maximum duration of one auto mode evaluation in milliseconds. Defaults to 30000 for either evaluation stage.";
        };
      };
    }
  );
in
{
  options.programs.lofi = {
    enable = lib.mkEnableOption "The Lofi coding agent";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.local.lofi;
      description = "The Lofi coding agent package to use.";
    };

    settings = lib.mkOption {
      type = tomlFormat.type;
      default = { };
      description = "Settings written to ~/.config/lofi/config.toml.";
    };

    permissions = lib.mkOption {
      type = lib.types.listOf permissionType;
      default = [ ];
      example = [
        {
          path = "/nix/store";
          access = "read";
        }
        {
          path = "~/notes";
          access = "write";
        }
      ];
      description = "Directories granted to the file tools outside the workspace root, written to [[permissions]] in config.toml.";
    };

    policy = lib.mkOption {
      type = lib.types.submodule {
        options = {
          rules = lib.mkOption {
            type = tomlFormat.type;
            default = { };
            description = "Shell policy rules (allow/ask/deny, wrappers, redirects, heredocs) written to policy.toml.";
          };
          auto_mode = lib.mkOption {
            type = autoModeType;
            default = null;
            description = "Auto mode configuration (enable/provider/model/decision_model/max_tokens/timeout_ms).";
          };
        };
      };
      default = { };
      description = "Shell policy written to ~/.config/lofi/policy.toml.";
    };

    instructionText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "The instruction text to use as the agent's AGENTS.md baseline.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.file = {
      ".config/lofi/AGENTS.md".text = cfg.instructionText;
      ".config/lofi/policy.toml".source = tomlFormat.generate "lofi-policy" (
        cfg.policy.rules
        // lib.optionalAttrs (cfg.policy.auto_mode != null) {
          auto_mode = lib.filterAttrs (_: value: value != null) cfg.policy.auto_mode;
        }
      );
    }
    // lib.optionalAttrs (cfg.settings != { } || cfg.permissions != [ ]) {
      ".config/lofi/config.toml".source = tomlFormat.generate "lofi-config" (
        cfg.settings // lib.optionalAttrs (cfg.permissions != [ ]) { permissions = cfg.permissions; }
      );
    };
  };
}
