{
  imports = [
    ../agents
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

  programs.git.ignores = [
    ".my/"

    # Agent files
    "HANDOFF.md"
    "LOCAL.md"
    "TODO.md"
  ];
}
