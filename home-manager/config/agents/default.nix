{
  imports = [
    ./domains.nix
    ./permissions.nix
    ./skills.nix

    # programs
    ../programs/agent-browser.nix
    ../programs/antigravity-cli
    ../programs/claude-code
    ../programs/codex
    ../programs/context7-cli.nix
    ../programs/exa-cli.nix
    ../programs/lsp-cli.nix
    ../programs/lofi
    ../programs/mcp.nix
    ../programs/pi-coding-agent
    ../programs/terminal-use.nix
    ../programs/tuicr.nix
  ];

  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';

  agents.requiredEnvs = [
    "ASANA_PAT" # var/skills/apis/asana
    "CLICKUP_PAT" # var/skills/apis/clickup
    "LINEAR_PAT" # var/skills/apis/linear
  ];

  programs.git.ignores = [
    ".my/"

    # Agent files
    "HANDOFF.md"
    "LOCAL.md"
    "TODO.md"
  ];
}
