{
  imports = [
    ../programs/claude-code.nix
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/mcp.nix
    ../programs/opencode.nix
    ../programs/pi-coding-agent.nix
  ];

  programs.codex.enable = true;

  programs.git.ignores = [ ".my/" ];
}
