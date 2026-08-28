{
  imports = [
    ./domains.nix
    ./permissions.nix
    ./skills.nix
  ];

  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';

  agents.requiredEnvs = [
    "ASANA_PAT" # var/skills/apis/asana
    "CLICKUP_PAT" # var/skills/apis/clickup
    "LINEAR_PAT" # var/skills/apis/linear
  ];
}
