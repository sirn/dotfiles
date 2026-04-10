{ lib, ... }:

{
  options.agents.domains.allowed = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "Allowed domains for WebFetch (consumed by Claude Code).";
  };
}
