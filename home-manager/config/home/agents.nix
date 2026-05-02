{
  imports = [
    ../agents
    ../programs/brave-search-cli.nix
    ../programs/claude-code
    ../programs/codex
    ../programs/gemini
    ../programs/mcp.nix
    ../programs/opencode
    ../programs/pi-coding-agent
    ../programs/rtk
    ../programs/web-cli.nix
  ];

  programs.git.ignores = [
    ".my/"

    # Agent files
    "HANDOFF.md"
    "LOCAL.md"
    "TODO.md"
  ];
}
