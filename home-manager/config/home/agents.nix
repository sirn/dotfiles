{
  imports = [
    ../agents
    ../programs/claude-code
    ../programs/gemini
    ../programs/lsp-cli.nix
    ../programs/mcp.nix
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
