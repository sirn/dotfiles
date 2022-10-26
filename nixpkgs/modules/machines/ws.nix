{
  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/kubeops.nix
    ../profiles/dev.nix

    # email
    ../programs/mbsync.nix
    ../programs/msmtp.nix
    ../programs/notmuch.nix

    # services
    ../runit/duplicity.nix
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/notmuch.nix
    ../runit/xlocate.nix
  ];
}
