{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.programs.lofi;
  tomlFormat = pkgs.formats.toml { };

  autoModeType = lib.types.nullOr (
    lib.types.submodule {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          description = "Whether to enable LLM-based auto mode pre-approval.";
        };
        provider = lib.mkOption {
          type = lib.types.str;
          example = "openai";
          description = "LLM provider used for auto mode approval.";
        };
        model = lib.mkOption {
          type = lib.types.str;
          example = "gpt-5.6-luna";
          description = "Model used for auto mode approval.";
        };
        max_tokens = lib.mkOption {
          type = lib.types.int;
          example = 256;
          description = "Maximum number of tokens for an auto mode response.";
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
            description = "Auto mode configuration (enable/provider/model/max_tokens).";
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
        // lib.optionalAttrs (cfg.policy.auto_mode != null) { auto_mode = cfg.policy.auto_mode; }
      );
    }
    // lib.optionalAttrs (cfg.settings != { }) {
      ".config/lofi/config.toml".source = tomlFormat.generate "lofi-config" cfg.settings;
    };
  };
}
