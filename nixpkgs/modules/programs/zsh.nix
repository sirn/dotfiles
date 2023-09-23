{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotfiles";

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

    initExtra = ''
      ${gpgScript}

      . ${dotfilesDir}/etc/zsh/share/ps1.zsh
      . ${dotfilesDir}/etc/zsh/functions/cd.zsh
      . ${dotfilesDir}/etc/zsh/functions/gg.zsh
      . ${dotfilesDir}/etc/zsh/functions/gq.zsh
      . ${dotfilesDir}/etc/zsh/functions/pcd.zsh
    '';
  };
}
