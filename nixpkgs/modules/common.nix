{ config, pkgs, ... }:

{
  imports = [
    ./home/email.nix
    ./home/xdg.nix
    ./home/shell.nix

    # programs
    ./programs/aria2.nix
    ./programs/bash.nix
    ./programs/coreutils.nix
    ./programs/curl.nix
    ./programs/direnv.nix
    ./programs/emacs.nix
    ./programs/fzf.nix
    ./programs/git.nix
    ./programs/gpg.nix
    ./programs/mercurial.nix
    ./programs/mosh.nix
    ./programs/neovim.nix
    ./programs/openssh.nix
    ./programs/password-store.nix
    ./programs/ripgrep.nix
    ./programs/rsync.nix
    ./programs/tmux.nix
    ./programs/unison.nix
    ./programs/weechat.nix
    ./programs/zsh.nix
  ];
}
