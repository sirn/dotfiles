{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.zsh = {
    enable = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;

    autocd = true;
    history = {
      expireDuplicatesFirst = true;
      ignoreDups = true;
    };

    initExtra = ''
      bindkey -e

      export SHELL=zsh
      export WORDCHARS="''${WORDCHARS/\//}"

      . ${dotfilesDir}/etc/zsh/share/ps1.zsh
      . ${dotfilesDir}/etc/zsh/functions/cd.zsh
      . ${dotfilesDir}/etc/zsh/functions/gg.zsh
      . ${dotfilesDir}/etc/zsh/functions/pcd.zsh
    '';
  };
}
