{
  imports = [
    ./home/email.nix
    ./home/xdg.nix
    ./home/shell.nix

    # programs
    ./programs/aria2.nix
    ./programs/aider-chat.nix
    ./programs/bash.nix
    ./programs/claude-code.nix
    ./programs/coreutils.nix
    ./programs/curl.nix
    ./programs/direnv.nix
    ./programs/emacs.nix
    ./programs/emacsc.nix
    ./programs/fish.nix
    ./programs/fzf.nix
    ./programs/git.nix
    ./programs/gpg.nix
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
    ./programs/sl.nix
    ./programs/tmux.nix
    ./programs/unison.nix

    # services
    ./services/emacs.nix
    ./services/gpg-agent.nix
  ];
}
