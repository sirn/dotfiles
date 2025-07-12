{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.gemini-cli
  ];

  programs.git = {
    ignores = [
      ".gemini/"
    ];
  };
}
