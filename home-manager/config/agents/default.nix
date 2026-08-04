{
  imports = [
    ./domains.nix
    ./permissions.nix
    ./skills.nix
  ];

  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';

  agents.requiredEnvs = [ "ASANA_PAT" ];
}
