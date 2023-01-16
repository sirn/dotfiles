{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  imports = [
    ../home/email.nix
    ../home/links.nix
    ../home/xdg.nix
    ../programs/aria2.nix
    ../programs/direnv.nix
    ../programs/emacs.nix
    ../programs/fzf.nix
    ../programs/git.nix
    ../programs/gpg.nix
    ../programs/htop.nix
    ../programs/imagemagick.nix
    ../programs/password-store.nix
    ../programs/s-tui.nix
    ../programs/tmux.nix
    ../programs/unison.nix
    ../programs/vim.nix
    ../programs/weechat.nix
  ];
}
