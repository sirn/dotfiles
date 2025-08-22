{ lib, pkgs, ... }:

let
  npxCodex = pkgs.writeScriptBin "codex" ''
    #!${pkgs.bash}/bin/bash
    # Runs Codex from Npx
    export PATH=${pkgs.nodejs_20}/bin:${pkgs.local.wrapped-uv}/bin:$PATH
    exec ${pkgs.nodejs_20}/bin/npx --yes @openai/codex "$@"
  '';
in
{
  home.packages = with pkgs; [
    npxCodex
  ];

  programs.git = {
    ignores = [
      ".codex/"
    ];
  };
}
