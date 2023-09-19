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
    ../programs/curl.nix
    ../programs/direnv.nix
    ../programs/emacs.nix
    ../programs/fzf.nix
    ../programs/git.nix
    ../programs/gpg.nix
    ../programs/mercurial.nix
    ../programs/mosh.nix
    ../programs/openssh.nix
    ../programs/password-store.nix
    ../programs/ripgrep.nix
    ../programs/rsync.nix
    ../programs/tmux.nix
    ../programs/unison.nix
    ../programs/vim.nix
    ../programs/weechat.nix
  ];
}
