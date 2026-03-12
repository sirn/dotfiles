{ lib, config, ... }:

let
  cfg = config.programs.claude-code;
in
{
  options.programs.claude-code.sandbox.enabled = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Enable sandbox for Claude Code.";
  };

  config = lib.mkIf (cfg.enable && cfg.sandbox.enabled) {
    programs.claude-code.settings.sandbox.enabled = true;
  };
}
