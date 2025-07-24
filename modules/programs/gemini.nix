{ lib, pkgs, ... }:

let
  npxGemini = pkgs.writeScriptBin "gemini" ''
    #!${pkgs.bash}/bin/bash
    # Runs Gemini from Npx
    PATH=${pkgs.nodejs_20}/bin:$PATH
    exec ${pkgs.nodejs_20}/bin/npx --yes @google/gemini-cli "$@"
  '';
in
{
  home.packages = with pkgs; [
    npxGemini
  ];

  programs.git = {
    ignores = [
      ".gemini/"
    ];
  };
}
