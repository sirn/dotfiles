{ lib, pkgs, ... }:

{
  options.programs.pi-coding-agent = {
    enable = lib.mkEnableOption "Pi coding agent";

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        List of extension file paths to load.
        Extensions are TypeScript modules that extend Pi's behavior.
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.local.pi-coding-agent;
      description = "The Pi coding agent package to use.";
    };

    instructionText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = ''
        The instruction text to use as AGENTS.md.
      '';
    };
  };
}
