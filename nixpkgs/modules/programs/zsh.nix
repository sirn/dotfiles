{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  gpgScript = lib.optionalString config.programs.gpg.enable ''
    GPG_TTY=$(tty)
    ${pkgs.gnupg}/bin/gpg-connect-agent updatestartuptty /bye >/dev/null
  '';
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

      ${gpgScript}

      . ${dotfilesDir}/etc/zsh/share/ps1.zsh
      . ${dotfilesDir}/etc/zsh/functions/cd.zsh
      . ${dotfilesDir}/etc/zsh/functions/gg.zsh
      . ${dotfilesDir}/etc/zsh/functions/pcd.zsh
    '';
  };
}
