{
  imports = [
    ../agents
    ../programs/claude-code
    ../programs/exa-cli.nix
    ../programs/gemini
    ../programs/lsp-cli.nix
    ../programs/mcp.nix
    ../programs/pi-coding-agent
    ../programs/rtk
    ../programs/synthetic-search-cli.nix
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
