{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.zsh = {
    enable = true;
    enableVteIntegration = true;

    syntaxHighlighting = {
      enable = true;
    };

    autocd = true;
    history = {
      append = true;
      expireDuplicatesFirst = true;
      ignoreAllDups = true;
      ignoreDups = true;
      save = 1000;
      size = 1000;
    };

    initExtra = ''
      export WORDCHARS="''${WORDCHARS/\//}"
      if [[ $- == *"i"* ]]; then
        export SHELL=${config.programs.zsh.package}/bin/zsh
      fi

      . ${dotfilesDir}/etc/zsh/share/ps1.zsh
      . ${dotfilesDir}/etc/zsh/functions/gg.zsh
      . ${dotfilesDir}/etc/zsh/functions/pcd.zsh
    '';
  };
}
