{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.bash}/bin/bash
      # Runs Gemini from Npx
      PATH=${pkgs.nodejs_20}/bin:${pkgs.uv}/bin:$PATH
      exec ${pkgs.nodejs_20}/bin/npx --yes @google/gemini-cli "$@"
    '')
  ];

  programs.git = {
    ignores = [
      ".gemini/"
    ];
  };
}
