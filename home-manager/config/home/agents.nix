{
  imports = [
    ../agents
    ../programs/agent-browser.nix
    ../programs/claude-code
    ../programs/context7-cli.nix
    ../programs/exa-cli.nix
    ../programs/lsp-cli.nix
    ../programs/mcp.nix
    ../programs/pi-coding-agent
    ../programs/tuicr.nix
  ];

  programs.git.ignores = [
    ".my/"

    # Agent files
    "HANDOFF.md"
    "LOCAL.md"
    "TODO.md"
  ];
}
