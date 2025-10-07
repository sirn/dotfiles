{
  imports = [
    ./home/clipboard.nix
    ./home/colors.nix
    ./home/email.nix
    ./home/llm-agent.nix
    ./home/shell.nix
    ./home/sops.nix
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
    ./programs/lstr.nix
    ./programs/magit.nix
    ./programs/mosh.nix
    ./programs/nix-index.nix
    ./programs/qalculate.nix
    ./programs/ripgrep.nix
    ./programs/rsync.nix
    ./programs/sl.nix
    ./programs/ssh.nix
    ./programs/tree.nix
    ./programs/zellij.nix

    # services
    ./services/emacs.nix
    ./services/gpg-agent.nix
    ./services/podman.nix
  ];
}
