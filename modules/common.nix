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
    ./programs/emacsc.nix
    ./programs/fzf.nix
    ./programs/git.nix
    ./programs/htop.nix
    ./programs/jq.nix
    ./programs/jujutsu.nix
    ./programs/magit.nix
    ./programs/mercurial.nix
    ./programs/mosh.nix
    ./programs/neovim.nix
    ./programs/nix-index.nix
    ./programs/openssh.nix
    ./programs/podman.nix
    ./programs/ripgrep.nix
    ./programs/rsync.nix
    ./programs/tmux.nix
    ./programs/unison.nix
    ./programs/zsh.nix

    # services
    ./services/emacs.nix
    ./services/ssh-agent.nix
  ];
}
