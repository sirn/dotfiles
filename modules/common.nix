{
  imports = [
    ./home/email.nix
    ./home/llm-agent.nix
    ./home/shell.nix
    ./home/xdg.nix

    # programs
    ./programs/aria2.nix
    ./programs/bash.nix
    ./programs/coreutils.nix
    ./programs/curl.nix
    ./programs/direnv.nix
    ./programs/emacs.nix
    ./programs/emacsc.nix
    ./programs/fd.nix
    ./programs/fish.nix
    ./programs/fzy.nix
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
    ./programs/ripgrep.nix
    ./programs/rsync.nix
    ./programs/sl.nix
    ./programs/ssh.nix
    ./programs/tmux.nix
    ./programs/tree.nix
    ./programs/unison.nix

    # services
    ./services/emacs.nix
    ./services/gpg-agent.nix
    ./services/podman.nix
  ];
}
