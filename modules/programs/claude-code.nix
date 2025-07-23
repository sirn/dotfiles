{ lib, pkgs, ... }:

{
  home.packages = [
    (pkgs.writeScriptBin "claude" ''
      #!${pkgs.bash}/bin/bash
      # Runs Claude Code from Npx
      PATH=${pkgs.nodejs_20}/bin:${pkgs.uv}/bin:$PATH
      exec ${pkgs.nodejs_20}/bin/npx --yes @anthropic-ai/claude-code "$@"
    '')
  ];

  programs.git = {
    ignores = [
      ".claude/"
    ];
  };
}
