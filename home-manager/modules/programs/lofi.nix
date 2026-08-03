{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.programs.lofi;
  tomlFormat = pkgs.formats.toml { };
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
      type = tomlFormat.type;
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
      ".config/lofi/policy.toml".source = tomlFormat.generate "lofi-policy" cfg.policy;
    }
    // lib.optionalAttrs (cfg.settings != { }) {
      ".config/lofi/config.toml".source = tomlFormat.generate "lofi-config" cfg.settings;
    };
  };
}
