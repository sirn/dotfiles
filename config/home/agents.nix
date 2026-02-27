{
  imports = [
    ../programs/claude-code
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/mcp.nix
    ../programs/opencode.nix
    ../programs/pi-coding-agent
  ];

  programs.codex.enable = true;

  programs.git.ignores = [ ".my/" ];
}
