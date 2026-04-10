{ lib, ... }:

{
  imports = [
    ./domains.nix
    ./models.nix
    ./permissions.nix
  ];

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
  };
}
