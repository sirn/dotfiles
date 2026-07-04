{ config, lib, ... }:

{
  config.home.shell.interactiveShell = "${lib.getExe config.programs.zsh.package}";

  imports = [
    ./home/agents.nix
    ./home/colors
    ./home/email.nix
    ./home/shell.nix
    ./home/sops.nix
    ./home/xdg.nix

    # programs
    ./programs/aria2.nix
    ./programs/coincide.nix
    ./programs/ast-grep.nix
    ./programs/attic.nix
    ./programs/atuin.nix
    ./programs/bash.nix
    ./programs/bat.nix
    ./programs/coreutils.nix
    ./programs/curl.nix
    ./programs/direnv.nix
    ./programs/emacs.nix
    ./programs/emacsc.nix
    ./programs/fd.nix
    ./programs/fzy.nix
    ./programs/gh.nix
    ./programs/git.nix
    ./programs/gpg.nix
    ./programs/htop.nix
    ./programs/jaq.nix
    ./programs/jujutsu.nix
    ./programs/jjui.nix
    ./programs/lstr.nix
    ./programs/less.nix
    ./programs/nix-index.nix
    ./programs/qalculate.nix
    ./programs/repoman.nix
    ./programs/ripgrep.nix
    ./programs/rsync.nix
    ./programs/sl.nix
    ./programs/ssh.nix
    ./programs/starship.nix
    ./programs/tincan.nix
    ./programs/tmux.nix
    ./programs/tree.nix
    ./programs/ghostty-terminfo.nix
    ./programs/wezterm-terminfo.nix
    ./programs/zsh.nix

    # services
    ./services/emacs.nix
    ./services/gpg-agent.nix
    ./services/podman.nix
  ];
}
