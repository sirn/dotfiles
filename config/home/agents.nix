{
  imports = [
    ../programs/claude-code
    ../programs/codex
    ../programs/gemini
    ../programs/mcp.nix
    ../programs/opencode
    ../programs/pi-coding-agent
  ];

  programs.codex.enable = true;

  programs.git.ignores = [ ".my/" ];
}
