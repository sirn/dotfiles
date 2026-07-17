{
  imports = [
    ./domains.nix
    ./permissions.nix
    ./skills.nix
  ];

  # Add additional tools section for additional insturction text injection
  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';
}
