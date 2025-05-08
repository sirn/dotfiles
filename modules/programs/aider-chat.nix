{ lib, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.aider-chat
  ];

  programs.git = {
    ignores = [
      ".aider*"
    ];
  };
}
