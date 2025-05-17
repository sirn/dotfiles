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

  home.file.".aider.conf.yml" = {
    text = ''
      dark-mode: true
      no-git: true
    '';
  };
}
