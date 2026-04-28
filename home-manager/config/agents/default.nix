{
  imports = [
    ./domains.nix
    ./models.nix
    ./permissions.nix
    ./skills.nix
    ./subagents
  ];

  # Add additional tools section for additional insturction text injection
  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';
}
