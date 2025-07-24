{ lib, pkgs, ... }:

let
  npxClaudeCode = pkgs.writeScriptBin "claude" ''
    #!${pkgs.bash}/bin/bash
    # Runs Claude Code from npx
    PATH=${pkgs.nodejs_20}/bin:$PATH
    exec ${pkgs.nodejs_20}/bin/npx --yes @anthropic-ai/claude-code "$@"
  '';
in
{
  home.packages = [
    npxClaudeCode
  ];

  programs.git = {
    ignores = [
      ".claude/"
    ];
  };
}
