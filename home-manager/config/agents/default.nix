{
  imports = [
    ./domains.nix
    ./models.nix
    ./permissions.nix
    ./skills.nix
    ./subagents
  ];

  agents.instructionText = builtins.readFile ../../../var/agents/AGENTS.md;
}
