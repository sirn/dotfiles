{
  imports = [
    ../agents
    ../programs/claude-code
    ../programs/context7-cli.nix
    ../programs/exa-cli.nix
    ../programs/lsp-cli.nix
    ../programs/mcp.nix
    ../programs/pi-coding-agent
    ../programs/agent-browser.nix
  ];

  programs.git.ignores = [
    ".my/"

    # Agent files
    "HANDOFF.md"
    "LOCAL.md"
    "TODO.md"
  ];
}
