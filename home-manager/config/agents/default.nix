{ lib, ... }:

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

  # Keep Additional tools just before tool-level lib.mkAfter (1500) bullets so
  # other instruction sections can insert between AGENTS.md (1000) and tools.
  agents.instructionText = lib.mkMerge [
    (lib.strings.trim (builtins.readFile ../../../var/agents/AGENTS.md))
    (lib.mkOrder 1400 "\n## Additional tools\n")
  ];

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
