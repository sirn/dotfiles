{
  imports = [
    ./domains.nix
    ./models.nix
    ./permissions.nix
    ./subagents
  ];

  agents.instructionText = builtins.readFile ../../../var/agents/instruction.md;
  agents.skillsDir = ../../../var/agents/skills;
}
