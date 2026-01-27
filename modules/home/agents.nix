{
  imports = [
    ../programs/claude-code.nix
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/mcp.nix
    ../programs/opencode.nix
  ];

  programs.git.ignores = [ ".my/" ];
}
